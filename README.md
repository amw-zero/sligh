# Description

Sligh is a language and toolchain for model-based testing. With Sligh, you write a simplified model of a system, and the compiler uses it to generate a test suite for an implementation system. The model becomes an executable specification that the test uses to determine correct behavior.

Here's a Sligh model of a simple counter application:

```
record Counter:
    name: Id(String)
    value: Int
end

process CounterApp:
  counters: Set(Counter)

  def GetCounters():
      counters
  end

  def CreateCounter(name: String):
      counters := counters.append(Counter.new(name, 0))
  end

  def Increment(name: String):
    def findCounter(counter: Counter):
        counter.name.equalsStr(name)
    end

    def updateCounter(counter: Counter):
        Counter.new(counter.name, counter.value + 1)
    end

    counters := counters.update(findCounter, updateCounter)
  end
end
```

The language itself is meant to be simple, since it's intended to only describe the high-level logic of a system. 

# Status

Sligh is an experiment and a prototype. The test compiler does function, but it currently assumes a very specific implementation setup: a Next.js application that uses Zustand for state management. The hope is to generalize the compiler so that different backends can be built to target different implementation architectures / patterns.

# Setup

```
brew install ocaml
brew install opam
opam install dune

dune build && dune install
```

# Usage

Generates a witness object intended to be used in a test generator.

```
sligh model.sl -cert <output file>
```
