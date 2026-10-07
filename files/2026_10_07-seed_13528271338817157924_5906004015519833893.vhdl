-- Seed: 13528271338817157924,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity kkradysxg is
  port (x : inout time; kiodjb : in std_logic_vector(1 to 2); cdyvzlvmtp : in real; hp : inout real);
end kkradysxg;

architecture ajtte of kkradysxg is
  
begin
  -- Single-driven assignments
  hp <= cdyvzlvmtp;
  x <= x;
end ajtte;

entity uoq is
  port (atqqacjlis : inout severity_level);
end uoq;

library ieee;
use ieee.std_logic_1164.all;

architecture hvwszslttd of uoq is
  signal zeoagtdiq : time;
  signal thg : real;
  signal exc : real;
  signal o : std_logic_vector(1 to 2);
  signal zn : time;
  signal fbmujsgls : time;
  signal iputb : real;
  signal xitso : real;
  signal sdnkr : std_logic_vector(1 to 2);
  signal vyn : time;
begin
  hliiurlhdc : entity work.kkradysxg
    port map (x => vyn, kiodjb => sdnkr, cdyvzlvmtp => xitso, hp => iputb);
  rjikvvhelf : entity work.kkradysxg
    port map (x => fbmujsgls, kiodjb => sdnkr, cdyvzlvmtp => xitso, hp => xitso);
  de : entity work.kkradysxg
    port map (x => zn, kiodjb => o, cdyvzlvmtp => exc, hp => thg);
  pnqolwu : entity work.kkradysxg
    port map (x => zeoagtdiq, kiodjb => o, cdyvzlvmtp => xitso, hp => exc);
  
  -- Single-driven assignments
  atqqacjlis <= ERROR;
end hvwszslttd;

entity kcyivleof is
  port (wvrv : out bit);
end kcyivleof;

library ieee;
use ieee.std_logic_1164.all;

architecture ym of kcyivleof is
  signal ihqicpd : real;
  signal inhml : std_logic_vector(1 to 2);
  signal lw : time;
  signal kaemtknuq : severity_level;
  signal hlyt : severity_level;
  signal x : severity_level;
begin
  kcnfydibt : entity work.uoq
    port map (atqqacjlis => x);
  kpayjeucf : entity work.uoq
    port map (atqqacjlis => hlyt);
  cmd : entity work.uoq
    port map (atqqacjlis => kaemtknuq);
  oenyrj : entity work.kkradysxg
    port map (x => lw, kiodjb => inhml, cdyvzlvmtp => ihqicpd, hp => ihqicpd);
  
  -- Single-driven assignments
  wvrv <= '1';
  
  -- Multi-driven assignments
  inhml <= inhml;
  inhml <= inhml;
end ym;

entity lmu is
  port (jwqqxd : buffer real);
end lmu;

architecture rdhgmfgpv of lmu is
  signal yif : bit;
begin
  i : entity work.kcyivleof
    port map (wvrv => yif);
  
  -- Single-driven assignments
  jwqqxd <= 33.0203;
end rdhgmfgpv;



-- Seed after: 12455437762873005235,5906004015519833893
