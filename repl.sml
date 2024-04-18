structure Repl = struct
  fun readline_opt () = TextIO.inputLine TextIO.stdIn handle _ => NONE
  val eval = Environment.run_line
  fun repl () =
    let
      open Signals
      val prev = setHandler (sigINT,IGNORE)
      val scope = Environment.new_scope ()
    in
      Std.callcc(fn exit => (
        Std.Table.set Basis.builtins "exit" (Basis.mkfun0 "exit" (Std.throw exit));
        Std.fix (fn f => fn () => (
          print ">>> ";
          Std.callcc (fn skip => (
            setHandler (sigINT, HANDLER (fn _ => Std.catch (fn () => (print "\nKeyboardInterrupt\n"; Std.throw skip ()))));
            eval scope (valOf (TextIO.inputLine TextIO.stdIn) handle _ => Std.throw exit ())
          ))
          ;f())
        ) ()
      ));
      Std.Table.rem Basis.builtins "exit";
      setHandler (sigINT,prev);
      ()
    end
end