/*******************************************************************************
 * Copyright (c) 2009 Luaj.org. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.luaj.vm2

import kotlinx.coroutines.yield
import org.luaj.vm2.internal.runBlockingNoSuspensions

typealias Instruction = Int
/**
 * Extension of [LuaFunction] which executes lua bytecode.
 *
 *
 * A [LuaClosure] is a combination of a [Prototype]
 * and a [LuaValue] to use as an environment for execution.
 * Normally the [LuaValue] is a [Globals] in which case the environment
 * will contain standard lua libraries.
 *
 *
 *
 * There are three main ways [LuaClosure] instances are created:
 *
 *  * Construct an instance using [.LuaClosure]
 *  * Construct it indirectly by loading a chunk via [Globals.load]
 *  * Execute the lua bytecode [Lua.OP_CLOSURE] as part of bytecode processing
 *
 *
 *
 * To construct it directly, the [Prototype] is typically created via a compiler such as
 * [org.luaj.vm2.compiler.LuaC]:
 * <pre> `String script = "print( 'hello, world' )";
 * InputStream is = new ByteArrayInputStream(script.getBytes());
 * Prototype p = LuaC.instance.compile(is, "script");
 * LuaValue globals = JsePlatform.standardGlobals();
 * LuaClosure f = new LuaClosure(p, globals);
 * f.call();
`</pre> *
 *
 *
 * To construct it indirectly, the [Globals.load] method may be used:
 * <pre> `Globals globals = JsePlatform.standardGlobals();
 * LuaFunction f = globals.load(new StringReader(script), "script");
 * LuaClosure c = f.checkclosure();  // This may fail if LuaJC is installed.
 * c.call();
`</pre> *
 *
 *
 * In this example, the "checkclosure()" may fail if direct lua-to-java-bytecode
 * compiling using LuaJC is installed, because no LuaClosure is created in that case
 * and the value returned is a [LuaFunction] but not a [LuaClosure].
 *
 *
 * Since a [LuaClosure] is a [LuaFunction] which is a [LuaValue],
 * all the value operations can be used directly such as:
 *
 *  * [LuaValue.call]
 *  * [LuaValue.call]
 *  * [LuaValue.invoke]
 *  * [LuaValue.invoke]
 *  * [LuaValue.method]
 *  * [LuaValue.method]
 *  * [LuaValue.invokemethod]
 *  * [LuaValue.invokemethod]
 *  *  ...
 *
 * @see LuaValue
 *
 * @see LuaFunction
 *
 * @see LuaValue.isclosure
 * @see LuaValue.checkclosure
 * @see LuaValue.optclosure
 * @see LoadState
 *
 * @see Globals.compiler
 */
class LuaClosure
/** Create a closure around a Prototype with a specific environment.
 * If the prototype has upvalues, the environment will be written into the first upvalue.
 * @param p the Prototype to construct this Closure for.
 * @param env the environment to associate with the closure.
 */
    (val p: Prototype, env: LuaValue?) : LuaFunction() {

    var upValues: Array<UpValue?> = when {
        p.upvalues.isEmpty() -> NOUPVALUES
        else -> arrayOfNulls<UpValue>(p.upvalues.size).also { it[0] = UpValue(arrayOf(env), 0) }
    }

    internal val globals: Globals? = if (env is Globals) env else null

    override fun isclosure(): Boolean = true
    override fun optclosure(defval: LuaClosure?): LuaClosure = this
    override fun checkclosure(): LuaClosure = this
    override fun getmetatable(): LuaValue? = s_metatable
    override fun tojstring(): String = "function: $p"

    override suspend fun callSuspend(): LuaValue {
        val stack = Array(p.maxstacksize) { NIL }
        return executeSuspend(stack, NONE).arg1()
    }

    override fun call(): LuaValue {
        val stack = Array(p.maxstacksize) { NIL }
        return execute(stack, NONE).arg1()
    }

    override fun call(arg: LuaValue): LuaValue {
        val stack = Array(p.maxstacksize) { NIL }
        when (p.numparams) {
            0 -> return execute(stack, arg).arg1()
            else -> {
                stack[0] = arg
                return execute(stack, NONE).arg1()
            }
        }
    }

    override fun call(arg1: LuaValue, arg2: LuaValue): LuaValue {
        val stack = Array(p.maxstacksize) { NIL }
        when (p.numparams) {
            1 -> {
                stack[0] = arg1
                return execute(stack, arg2).arg1()
            }

            0 -> return execute(stack, if (p.is_vararg != 0) varargsOf(arg1, arg2) else NONE).arg1()
            else -> {
                stack[0] = arg1
                stack[1] = arg2
                return execute(stack, NONE).arg1()
            }
        }
    }

    override fun call(arg1: LuaValue, arg2: LuaValue, arg3: LuaValue): LuaValue {
        val stack = Array(p.maxstacksize) { NIL }
        return when (p.numparams) {
            0 -> execute(stack, if (p.is_vararg != 0) varargsOf(arg1, arg2, arg3) else NONE).arg1()
            1 -> {
                stack[0] = arg1
                execute(stack, if (p.is_vararg != 0) varargsOf(arg2, arg3) else NONE).arg1()
            }

            2 -> {
                stack[0] = arg1
                stack[1] = arg2
                execute(stack, arg3).arg1()
            }

            else -> {
                stack[0] = arg1
                stack[1] = arg2
                stack[2] = arg3
                execute(stack, NONE).arg1()
            }
        }
    }

    override suspend fun invokeSuspend(args: Varargs): Varargs {
        return onInvokeSuspend(args).evalSuspend()
    }

    override suspend fun onInvokeSuspend(args: Varargs): Varargs {
        val stack = Array(p.maxstacksize) { i -> args.arg(i + 1) }
        return executeSuspend(stack, if (p.is_vararg != 0) args.subargs(p.numparams + 1) else NONE)
    }

    override fun invoke(args: Varargs): Varargs = onInvoke(args).eval()

    override fun onInvoke(args: Varargs): Varargs {
        val stack = Array(p.maxstacksize) { i -> args.arg(i + 1) }
        return execute(stack, if (p.is_vararg != 0) args.subargs(p.numparams + 1) else NONE)
    }

    protected fun execute(stack: Array<LuaValue>, varargs: Varargs): Varargs {
        return runBlockingNoSuspensions {
            executeSuspend(stack, varargs)
        }
    }

    protected suspend fun executeSuspend(stack: Array<LuaValue>, varargs: Varargs): Varargs {
        // loop through instructions
        var pc = 0
        var top = 0
        var v: Varargs = NONE
        val code = p.code
        val k = p.k

        // upvalues are only possible when closures create closures
        // TODO: use linked list.
        val openups = if (p.p.isNotEmpty()) arrayOfNulls<UpValue>(stack.size) else null

        // allow for debug hooks
        globals?.debuglib?.onCall(this, varargs, stack)

        // Get the value from register, from the constants array or from stack
        // regarding the value of the index.
        fun RK(index: Int): LuaValue {
            return if (index > 0xff) k[index and 0x0ff] else stack[index]
        }

        // process instructions
        try {
            loop@ while (true) {
                globals?.debuglib?.onInstruction(pc, v, top)

                // pull out instruction
                val i = code[pc]

                // process the op code
                when (opcode(i)) {

                    Lua.OP_MOVE/*	A B	R(A):= R(B)					*/ -> {
                        stack[A(i)] = stack[B(i)]
                        ++pc
                        continue@loop
                    }

                    Lua.OP_LOADK/*	A Bx	R(A):= Kst(Bx)					*/ -> {
                        stack[A(i)] = k[Bx(i)]
                        ++pc
                        continue@loop
                    }

                    Lua.OP_LOADBOOL/*	A B C	R(A):= (Bool)B: if (C) pc++			*/ -> {
                        stack[A(i)] = if (B(i) != 0) {
                            BTRUE
                        } else {
                            BFALSE
                        }
                        if (C(i) != 0) {
                            ++pc /* skip next instruction (if C) */
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_LOADNIL /*	A B	R(A):= ...:= R(A+B):= nil			*/ -> {
                        var a = A(i)
                        var b = B(i)
                        while (b-- >= 0)
                            stack[a++] = LuaValue.NIL
                        ++pc
                        continue@loop
                    }

                    Lua.OP_GETUPVAL /*	A B	R(A):= UpValue[B]				*/ -> {
                        stack[A(i)] = upValues[B(i)]!!.value!!
                        ++pc
                        continue@loop
                    }

                    Lua.OP_GETTABUP /*	A B C	R(A) := UpValue[B][RK(C)]			*/ -> {
                        stack[A(i)] = upValues[B(i)]!!.value!![RK(C(i))]
                        ++pc
                        continue@loop
                    }

                    Lua.OP_GETTABLE /*	A B C	R(A):= R(B)[RK(C)]				*/ -> {
                        stack[A(i)] = stack[B(i)][RK(C(i))]
                        ++pc
                        continue@loop
                    }

                    Lua.OP_SETTABUP /*	A B C	UpValue[A][RK(B)] := RK(C)			*/ -> {
                        upValues[A(i)]!!.value!![RK(B(i))] = RK(C(i))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_SETUPVAL /*	A B	UpValue[B]:= R(A)				*/ -> {
                        upValues[B(i)]?.value = stack[A(i)]
                        ++pc
                        continue@loop
                    }

                    Lua.OP_SETTABLE /*	A B C	R(A)[RK(B)]:= RK(C)				*/ -> {
                        stack[A(i)][RK(B(i))] = RK(C(i))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_NEWTABLE /*	A B C	R(A):= {} (size = B,C)				*/ -> {
                        stack[A(i)] = LuaTable(B(i), C(i))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_SELF /*	A B C	R(A+1):= R(B): R(A):= R(B)[RK(C)]		*/ -> {
                        val o = stack[B(i)]
                        stack[A(i) + 1] = o
                        stack[A(i)] = o[RK(C(i))]
                        ++pc
                        continue@loop
                    }

                    Lua.OP_ADD /*	A B C	R(A):= RK(B) + RK(C)				*/ -> {
                        stack[A(i)] = RK(B(i)).add(RK(C(i)))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_SUB /*	A B C	R(A):= RK(B) - RK(C)				*/ -> {
                        stack[A(i)] = RK(B(i)).sub(RK(C(i)))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_MUL /*	A B C	R(A):= RK(B) * RK(C)				*/ -> {
                        stack[A(i)] = RK(B(i)).mul(RK(C(i)))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_DIV /*	A B C	R(A):= RK(B) / RK(C)				*/ -> {
                        stack[A(i)] = RK(B(i)).div(RK(C(i)))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_MOD /*	A B C	R(A):= RK(B) % RK(C)				*/ -> {
                        stack[A(i)] = RK(B(i)).mod(RK(C(i)))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_POW /*	A B C	R(A):= RK(B) ^ RK(C)				*/ -> {
                        stack[A(i)] = (RK(B(i))).pow(RK(C(i)))
                        ++pc
                        continue@loop
                    }

                    Lua.OP_UNM /*	A B	R(A):= -R(B)					*/ -> {
                        stack[A(i)] = stack[B(i)].neg()
                        ++pc
                        continue@loop
                    }

                    Lua.OP_NOT /*	A B	R(A):= not R(B)				*/ -> {
                        stack[A(i)] = stack[B(i)].not()
                        ++pc
                        continue@loop
                    }

                    Lua.OP_LEN /*	A B	R(A):= length of R(B)				*/ -> {
                        stack[A(i)] = stack[B(i)].len()
                        ++pc
                        continue@loop
                    }

                    Lua.OP_CONCAT /*	A B C	R(A):= R(B).. ... ..R(C)			*/ -> {
                        val b = B(i)
                        var c = C(i)
                        run {
                            if (c > b + 1) {
                                var sb = stack[c].buffer()
                                while (--c >= b)
                                    sb = stack[c].concat(sb)
                                stack[A(i)] = sb.value()
                            } else {
                                stack[A(i)] = stack[c - 1].concat(stack[c])
                            }
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_JMP /*	sBx	pc+=sBx					*/ -> {
                        pc += Bx(i) - 0x1ffff
                        var a = A(i)
                        if (a > 0) {
                            --a
                            var b = openups!!.size
                            while (--b >= 0)
                                if (openups[b] != null && openups[b]!!.index >= a) {
                                    openups[b]!!.close()
                                    openups[b] = null
                                }
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_EQ /*	A B C	if ((RK(B) == RK(C)) ~= A) then pc++		*/ -> {
                        val a = A(i)
                        if ((RK(B(i))).eq_b(
                                RK(C(i))
                            ) != (a != 0)
                        )
                            ++pc
                        ++pc
                        continue@loop
                    }

                    Lua.OP_LT /*	A B C	if ((RK(B) <  RK(C)) ~= A) then pc++  		*/ -> {
                        val a = A(i)
                        if ((RK(B(i))).lt_b(
                                RK(C(i))
                            ) != (a != 0)
                        )
                            ++pc
                        ++pc
                        continue@loop
                    }

                    Lua.OP_LE /*	A B C	if ((RK(B) <= RK(C)) ~= A) then pc++  		*/ -> {
                        val a = A(i)
                        if ((RK(B(i))).lteq_b(RK(C(i))) != (a != 0))
                            ++pc
                        ++pc
                        continue@loop
                    }

                    Lua.OP_TEST /*	A C	if not (R(A) <=> C) then pc++			*/ -> {
                        if (stack[A(i)].toboolean() != (C(i) != 0))
                            ++pc
                        ++pc
                        continue@loop
                    }

                    Lua.OP_TESTSET /*	A B C	if (R(B) <=> C) then R(A):= R(B) else pc++	*/ -> {
                        /* note: doc appears to be reversed */
                        val o = stack[B(i)]
                        if (o.toboolean() != (C(i) != 0))
                            ++pc
                        else
                            stack[A(i)] = o // TODO: should be sBx?
                        ++pc
                        continue@loop
                    }

                    Lua.OP_CALL /*	A B C	R(A), ... ,R(A+C-2):= R(A)(R(A+1), ... ,R(A+B-1)) */ -> when (i and (Lua.MASK_B or Lua.MASK_C)) {
                        1 shl Lua.POS_B or (0 shl Lua.POS_C) -> {
                            val a = A(i)
                            v = stack[a].invokeSuspend(NONE)
                            top = a + v.narg()
                            ++pc
                            continue@loop
                        }

                        2 shl Lua.POS_B or (0 shl Lua.POS_C) -> {
                            val a = A(i)
                            v = stack[a].invokeSuspend(stack[a + 1])
                            top = a + v.narg()
                            ++pc
                            continue@loop
                        }

                        1 shl Lua.POS_B or (1 shl Lua.POS_C) -> {
                            stack[A(i)].callSuspend()
                            ++pc
                            continue@loop
                        }

                        2 shl Lua.POS_B or (1 shl Lua.POS_C) -> {
                            val a = A(i)
                            stack[a].callSuspend(stack[a + 1])
                            ++pc
                            continue@loop
                        }

                        3 shl Lua.POS_B or (1 shl Lua.POS_C) -> {
                            val a = A(i)
                            stack[a].callSuspend(stack[a + 1], stack[a + 2])
                            ++pc
                            continue@loop
                        }

                        4 shl Lua.POS_B or (1 shl Lua.POS_C) -> {
                            val a = A(i)
                            stack[a].callSuspend(stack[a + 1], stack[a + 2], stack[a + 3])
                            ++pc
                            continue@loop
                        }

                        1 shl Lua.POS_B or (2 shl Lua.POS_C) -> {
                            stack[A(i)] = stack[A(i)].callSuspend()
                            ++pc
                            continue@loop
                        }

                        2 shl Lua.POS_B or (2 shl Lua.POS_C) -> {
                            val a = A(i)
                            stack[a] = stack[A(i)].callSuspend(stack[a + 1])
                            ++pc
                            continue@loop
                        }

                        3 shl Lua.POS_B or (2 shl Lua.POS_C) -> {
                            val a = A(i)
                            stack[A(i)] = stack[a].callSuspend(stack[a + 1], stack[a + 2])
                            ++pc
                            continue@loop
                        }

                        4 shl Lua.POS_B or (2 shl Lua.POS_C) -> {
                            val a = A(i)
                            stack[A(i)] = stack[a].callSuspend(stack[a + 1], stack[a + 2], stack[a + 3])
                            ++pc
                            continue@loop
                        }

                        else -> {
                            val a = A(i)
                            val b = B(i)
                            val c = C(i)
                            v = stack[a].invokeSuspend(
                                if (b > 0)
                                    varargsOf(stack, a + 1, b - 1)
                                else
                                // exact arg count
                                    varargsOf(stack, a + 1, top - v.narg() - (a + 1), v)
                            )  // from prev top
                            if (c > 0) {
                                v.copyto(stack, a, c - 1)
                                v = NONE
                            } else {
                                top = a + v.narg()
                                v = v.dealias()
                            }
                            ++pc
                            continue@loop
                        }
                    }

                    Lua.OP_TAILCALL /*	A B C	return R(A)(R(A+1), ... ,R(A+B-1))		*/ -> {
                        val a = A(i)

                        when (i and Lua.MASK_B) {
                            1 shl Lua.POS_B -> {
                                return TailcallVarargs(stack[a], NONE)
                            }

                            2 shl Lua.POS_B -> return TailcallVarargs(stack[a], stack[a + 1])
                            3 shl Lua.POS_B -> return TailcallVarargs(
                                stack[a],
                                varargsOf(stack[a + 1], stack[a + 2])
                            )

                            4 shl Lua.POS_B -> return TailcallVarargs(
                                stack[a],
                                varargsOf(stack[a + 1], stack[a + 2], stack[a + 3])
                            )

                            else -> {
                                val b = B(i)
                                v = if (b > 0)
                                    varargsOf(stack, a + 1, b - 1)
                                else
                                // exact arg count
                                    varargsOf(stack, a + 1, top - v.narg() - (a + 1), v) // from prev top
                                return TailcallVarargs(stack[A(i)], v)
                            }
                        }
                    }

                    Lua.OP_RETURN /*	A B	return R(A), ... ,R(A+B-2)	(see note)	*/ -> {
                        val a = A(i)
                        val b = B(i)
                        when (b) {
                            0 -> return varargsOf(stack, a, top - v.narg() - a, v)
                            1 -> return NONE
                            2 -> return stack[A(i)]
                            else -> return varargsOf(stack, a, b - 1)
                        }
                    }

                    Lua.OP_FORLOOP /*	A sBx	R(A)+=R(A+2): if R(A) <?= R(A+1) then { pc+=sBx: R(A+3)=R(A) }*/ -> {
                        val a = A(i)
                        val limit = stack[a + 1]
                        val step = stack[a + 2]
                        val idx = step.add(stack[A(i)])
                        if (if (step.gt_b(0)) idx.lteq_b(limit) else idx.gteq_b(limit)) {
                            stack[A(i)] = idx
                            stack[a + 3] = idx
                            pc += Bx(i) - 0x1ffff
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_FORPREP /*	A sBx	R(A)-=R(A+2): pc+=sBx				*/ -> {
                        val a = A(i)
                        val init = stack[a].checknumber("'for' initial value must be a number")
                        val limit = stack[a + 1].checknumber("'for' limit must be a number")
                        val step = stack[a + 2].checknumber("'for' step must be a number")
                        stack[A(i)] = init.sub(step)
                        stack[a + 1] = limit
                        stack[a + 2] = step
                        pc += Bx(i) - 0x1ffff
                        ++pc
                        continue@loop
                    }

                    Lua.OP_TFORCALL /* A C	R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));	*/ -> {
                        val a = A(i)
                        v = stack[a].invokeSuspend(varargsOf(stack[a + 1], stack[a + 2]))
                        var c = C(i)
                        while (--c >= 0)
                            stack[a + 3 + c] = v.arg(c + 1)
                        v = NONE
                        ++pc
                        continue@loop
                    }

                    Lua.OP_TFORLOOP /* A sBx	if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx */ -> {
                        val a = A(i)
                        if (!stack[a + 1].isnil()) { /* continue loop? */
                            stack[A(i)] = stack[a + 1]  /* save control varible. */
                            pc += Bx(i) - 0x1ffff
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_SETLIST /*	A B C	R(A)[(C-1)*FPF+i]:= R(A+i), 1 <= i <= B	*/ -> {
                        val a = A(i)
                        var b = B(i)
                        var c = C(i)
                        if (c == 0) {
                            c = code[++pc]
                        }
                        val offset = (c - 1) * Lua.LFIELDS_PER_FLUSH
                        val o = stack[A(i)]
                        if (b == 0) {
                            b = top - a - 1
                            val m = b - v.narg()
                            var j = 1
                            while (j <= m) {
                                o[offset + j] = stack[a + j]
                                j++
                            }
                            while (j <= b) {
                                o[offset + j] = v.arg(j - m)
                                j++
                            }
                        } else {
                            o.presize(offset + b)
                            for (j in 1..b)
                                o[offset + j] = stack[a + j]
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_CLOSURE /*	A Bx	R(A):= closure(KPROTO[Bx])	*/ -> {
                        val newp = p.p[Bx(i)]
                        val ncl = LuaClosure(newp, globals)
                        val uv = newp.upvalues
                        var j = 0
                        val nup = uv.size
                        while (j < nup) {
                            if (uv[j].instack)
                            /* upvalue refes to local variable? */
                                ncl.upValues[j] = findupval(stack, uv[j].idx, openups!!)
                            else
                            /* get upvalue from enclosing function */
                                ncl.upValues[j] = upValues[uv[j].idx.toInt()]
                            ++j
                        }
                        stack[A(i)] = ncl
                        ++pc
                        continue@loop
                    }

                    Lua.OP_VARARG /*	A B	R(A), R(A+1), ..., R(A+B-1) = vararg		*/ -> {
                        val a = A(i)
                        val b = B(i)
                        if (b == 0) {
                            top = a + varargs.narg()
                            v = varargs
                        } else {
                            for (j in 1 until b)
                                stack[a + j - 1] = varargs.arg(j)
                        }
                        ++pc
                        continue@loop
                    }

                    Lua.OP_EXTRAARG -> throw IllegalArgumentException("Uexecutable opcode: OP_EXTRAARG")

                    else -> throw IllegalArgumentException("Illegal opcode: " + opcode(i))
                }
            }
        } catch (le: LuaError) {
            if (le.traceback == null) processErrorHooks(le, p, pc)
            throw le
        } catch (e: Exception) {
            val le = LuaError(e)
            processErrorHooks(le, p, pc)
            throw le
        } finally {
            if (openups != null) {
                var u = openups.size
                while (--u >= 0) if (openups[u] != null) openups[u]!!.close()
            }
            globals?.debuglib?.onReturn()
        }
    }

    /**
     * Extract the opcode from the instruction
     */
    private fun opcode(i: Instruction) = i and 0x3f

    /**
     * Extract the register C value from the instruction
     */
    private fun C(i: Instruction) = i shr 14 and 0x1ff

    /**
     * Extract the register Bx value from the instruction
     */
    private fun Bx(i: Instruction) = i.ushr(14)

    /**
     * Extract the register B value from the instruction
     */
    private fun B(i: Instruction) = i.ushr(23)

    /**
     * Extract the register A value from the instruction
     */
    private fun A(i: Instruction) = i shr 6 and 0xff

    /**
     * Run the error hook if there is one
     * @param msg the message to use in error hook processing.
     */
    internal fun errorHook(msg: String, level: Int): String {
        if (globals == null) return msg
        val r = globals.running
        if (r.errorfunc == null) return globals.debuglib?.let { msg + "\n" + it.traceback(level) } ?: msg
        val e = r.errorfunc
        r.errorfunc = null
        return try {
            e!!.call(valueOf(msg)).tojstring()
        } catch (t: Throwable) {
            "error in error handling"
        } finally {
            r.errorfunc = e
        }
    }

    private fun processErrorHooks(le: LuaError, p: Prototype, pc: Int) {
        val scriptName = p.source.tojstring()
        val lineinfo = if(pc >= 0 && pc < p.lineinfo.size) {
            p.lineinfo[pc]
        } else {
            null
        }

        le.fileline = (scriptName + ":" + (lineinfo?.toString() ?: "?"))
        le.line = lineinfo ?: 0
        le.script = scriptName
        le.traceback = errorHook(le.message!!, le.level)
    }

    @Suppress("UNCHECKED_CAST")
    private fun findupval(stack: Array<LuaValue>, idx: Short, openups: Array<UpValue?>): UpValue? {
        val n = openups.size
        for (i in 0 until n) if (openups[i] != null && openups[i]!!.index == idx.toInt()) return openups[i]
        for (i in 0 until n) if (openups[i] == null) return UpValue(stack as Array<LuaValue?>, idx.toInt()).also { openups[i] = it }
        LuaValue.error("No space for upvalue")
        return null
    }

    override fun name(): String = "<" + p.shortsource() + ":" + p.linedefined + ">"

    companion object {
        private val NOUPVALUES = arrayOfNulls<UpValue>(0)
    }
}
