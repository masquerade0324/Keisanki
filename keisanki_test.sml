structure KeisankiTest =
struct

  structure T = KeisankiTokens
  structure KL = KeisankiLexer
  structure KP = KeisankiParseFn(KL)

  fun tok2s (T.ID s)  = s
    | tok2s (T.NUM n) = Int.toString n
    | tok2s tok     = KeisankiTokens.toString tok

  fun keisan instrm =
    let
      val sm   = AntlrStreamPos.mkSourcemap ()
      val lex  = KL.lex sm
      val strm = KL.streamifyInstream instrm
      val (r, strm', errs) = KP.parse lex AtomMap.empty strm
    in
      print (String.concatWith "\n"
              (map (AntlrRepair.repairToString tok2s sm) errs));
      r
    end
end
