# How do is parsed is a bit of a mystery, so here are some test cases...

command do():
  discard 0

# These two forms _generally_ generate the same AST, significantly different from the one above
command:
  discard 1
command:
  discard 2

command "param" do():
  discard 3
# for some reason, this comes with an extra nkCall in the AST
command "param" do:
  discard 4
command "param":
  discard 5

call do():
  discard 6
call:
  discard 7
call:
  discard 8

call("param") do():
  discard 9
call("param"):
  discard 10
call("param"):
  discard 11

call.dotExpr do():
  discard 12
call.dotExpr:
  discard 13
call.dotExpr:
  discard 14

asgn = command do():
  discard 15
asgn = command:
  discard 16
asgn = command:
  discard 17

asgn = command do():
  discard 18
asgn = command:
  discard 19
asgn = command:
  discard 20

if false:
  return command do():
    discard 21

if false:
  return command:
    discard 22

if false:
  return command:
    discard 23

discard command do():
  discard 24

discard command:
  discard 25

discard command:
  discard 26

call(
  command do():
    27
)
call(
  command do:
    28
)
call(command: 29)

command "llllllllllllllllooooooooooooooooooonnnnnnnnnnnnnnnnnnnnnnggggggggggggggggggggg parameter":
  discard

command param:
of a:
  discard
else:
  discard

discard (
  aaa.bbb.exec do(res: int64):
    size = res).ccc()

macro `->`(a, b, c: untyped) =
  discard

1 -> 2 do:
  discard
