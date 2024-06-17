package org.luaj.vm2.lib

import org.luaj.vm2.LuaClosure
import org.luaj.vm2.LuaFunction
import org.luaj.vm2.LuaValue
import org.luaj.vm2.Varargs

interface ExecutionListener {
    fun onCall(f: LuaFunction)

    suspend fun onCall(c: LuaClosure, varargs: Varargs, stack: Array<LuaValue>)

    suspend fun onInstruction(pc: Int, v: Varargs, top: Int)

    fun onReturn()

    fun traceback(level: Int): String
}