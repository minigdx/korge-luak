plugins {
    id("com.github.minigdx.gradle.plugin.developer.mpp")
}

dependencies {
    add("commonMainApi", "org.jetbrains.kotlinx:kotlinx-coroutines-core:1.10.2")
}

minigdxDeveloper {
    this.name.set("luak")
    this.description.set("LUAK - Kotlin port of LuaJ (fork of https://github.com/korlibs/korge-luak)")
    this.projectUrl.set("https://github.com/minigdx/korge-luak")
    this.licence {
        name.set("MIT Licence")
        url.set("https://github.com/minigdx/korge-luak/blob/main/LICENSE")
    }
    developer {
        name.set("Carlos Ballesteros Velasco")
        url.set("https://github.com/soywiz")
    }
    developer {
        name.set("David Wursteisen")
        email.set("david.wursteisen+minigdx@gmail.com")
        url.set("https://github.com/dwursteisen")
    }
}
