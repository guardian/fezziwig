namespace scala com.gu.fezziwig

struct StructC {
  1: required string s
}

struct StructD {
  1: required string s
}

union Union1 {
  1: StructC c
  2: StructD d
}

enum Enum {
  ENUM_A = 0
  ENUM_B = 1
}

struct StructB {
  1: required Union1 u
}

struct StructA {
  1: required StructB b
  2: optional string foo
  3: optional i32 bar
  4: required Enum e
  5: required map<string, list<i32>> intMap
  7: optional StructC x
}

struct DefaultTestStruct {
  1: optional i32 first
  2: optional i32 second = 2
  3: i32 third
  4: i32 fourth = 4
  5: required i32 fifth
  6: required i32 sixth = 6
  7: required list<i32> seventh
}
