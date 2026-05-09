-- Seed: 16748415140159011443,11060400121348610183



entity nlzlvlw is
  port (jk : linkage time; ttmfr : out real; sysdb : linkage real; qrmajdhmds : out time);
end nlzlvlw;



architecture otmabub of nlzlvlw is
  
begin
  
end otmabub;



entity eadqsixg is
  port (vmgqsvil : in real; glr : out integer; exrnwcetx : inout severity_level);
end eadqsixg;



architecture gvcxvat of eadqsixg is
  signal qwrfucato : time;
  signal qukipgoitw : real;
  signal iqgu : time;
  signal cwfode : time;
  signal ouxpvcuzda : real;
  signal snc : real;
  signal rwalvsk : time;
begin
  lhf : entity work.nlzlvlw
    port map (jk => rwalvsk, ttmfr => snc, sysdb => ouxpvcuzda, qrmajdhmds => cwfode);
  nebay : entity work.nlzlvlw
    port map (jk => cwfode, ttmfr => ouxpvcuzda, sysdb => snc, qrmajdhmds => rwalvsk);
  fhx : entity work.nlzlvlw
    port map (jk => iqgu, ttmfr => qukipgoitw, sysdb => vmgqsvil, qrmajdhmds => qwrfucato);
end gvcxvat;

library ieee;
use ieee.std_logic_1164.all;

entity kocupttoju is
  port (vb : linkage std_logic; foogxkr : inout boolean; gtxp : linkage time; uikhmrvmef : linkage time);
end kocupttoju;



architecture lxeym of kocupttoju is
  signal p : real;
  signal lmrsvmbxx : real;
  signal lymq : time;
  signal wu : time;
  signal evsuok : severity_level;
  signal gkwkogxpv : integer;
  signal zqapxvz : real;
  signal fkahhpxfdt : severity_level;
  signal byheloumci : integer;
  signal c : real;
begin
  vpmu : entity work.eadqsixg
    port map (vmgqsvil => c, glr => byheloumci, exrnwcetx => fkahhpxfdt);
  yvzfzx : entity work.eadqsixg
    port map (vmgqsvil => zqapxvz, glr => gkwkogxpv, exrnwcetx => evsuok);
  mruwx : entity work.nlzlvlw
    port map (jk => wu, ttmfr => c, sysdb => c, qrmajdhmds => lymq);
  dd : entity work.nlzlvlw
    port map (jk => uikhmrvmef, ttmfr => lmrsvmbxx, sysdb => p, qrmajdhmds => wu);
end lxeym;



entity xttzbnznhw is
  port (wqvgewdvms : buffer severity_level);
end xttzbnznhw;

library ieee;
use ieee.std_logic_1164.all;

architecture ubujwljgyq of xttzbnznhw is
  signal oipdjmz : real;
  signal lxclxa : time;
  signal etmjleqayz : boolean;
  signal wslcvp : std_logic;
  signal ahwmq : time;
  signal leg : real;
  signal udypqfl : time;
begin
  es : entity work.nlzlvlw
    port map (jk => udypqfl, ttmfr => leg, sysdb => leg, qrmajdhmds => ahwmq);
  kmqebbwwoi : entity work.kocupttoju
    port map (vb => wslcvp, foogxkr => etmjleqayz, gtxp => lxclxa, uikhmrvmef => udypqfl);
  ny : entity work.nlzlvlw
    port map (jk => ahwmq, ttmfr => oipdjmz, sysdb => leg, qrmajdhmds => udypqfl);
end ubujwljgyq;



-- Seed after: 14901774881674872774,11060400121348610183
