-- Seed: 17890316467885167207,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity vge is
  port (jahrhfoh : out bit; ntpnhkknfw : buffer std_logic_vector(2 to 3); fhemn : in time);
end vge;

architecture rutufbm of vge is
  
begin
  -- Single-driven assignments
  jahrhfoh <= '1';
  
  -- Multi-driven assignments
  ntpnhkknfw <= "1W";
  ntpnhkknfw <= ('X', 'Z');
end rutufbm;

entity wbmqqc is
  port (pdgxbhnwty : in real; dfvnbteel : in character; ujfobbnc : in time);
end wbmqqc;

library ieee;
use ieee.std_logic_1164.all;

architecture crbfk of wbmqqc is
  signal hiltvw : time;
  signal frmc : bit;
  signal yrr : time;
  signal ybzrnyneej : std_logic_vector(2 to 3);
  signal biirvsdyuu : bit;
  signal vvunpirdgy : time;
  signal urnkofam : std_logic_vector(2 to 3);
  signal n : bit;
begin
  ih : entity work.vge
    port map (jahrhfoh => n, ntpnhkknfw => urnkofam, fhemn => vvunpirdgy);
  fhdfv : entity work.vge
    port map (jahrhfoh => biirvsdyuu, ntpnhkknfw => ybzrnyneej, fhemn => yrr);
  l : entity work.vge
    port map (jahrhfoh => frmc, ntpnhkknfw => urnkofam, fhemn => hiltvw);
  
  -- Single-driven assignments
  vvunpirdgy <= 3 min;
  yrr <= 0_4 ns;
  
  -- Multi-driven assignments
  urnkofam <= "HZ";
  urnkofam <= ('L', '0');
end crbfk;

library ieee;
use ieee.std_logic_1164.all;

entity kgfobmni is
  port (xgprdozg : linkage std_logic; dmrlcz : linkage time; jwvmtnu : out integer; cqwo : out bit);
end kgfobmni;

library ieee;
use ieee.std_logic_1164.all;

architecture uhsaplhtp of kgfobmni is
  signal mfao : std_logic_vector(2 to 3);
  signal s : std_logic_vector(2 to 3);
  signal asqomfixkw : bit;
  signal kuoxdwmwq : std_logic_vector(2 to 3);
  signal ducad : bit;
  signal nnk : time;
  signal bv : std_logic_vector(2 to 3);
  signal oodhfzulez : bit;
begin
  mjuyql : entity work.vge
    port map (jahrhfoh => oodhfzulez, ntpnhkknfw => bv, fhemn => nnk);
  wce : entity work.vge
    port map (jahrhfoh => ducad, ntpnhkknfw => kuoxdwmwq, fhemn => nnk);
  l : entity work.vge
    port map (jahrhfoh => asqomfixkw, ntpnhkknfw => s, fhemn => nnk);
  rcgepooyn : entity work.vge
    port map (jahrhfoh => cqwo, ntpnhkknfw => mfao, fhemn => nnk);
  
  -- Single-driven assignments
  jwvmtnu <= 1;
  nnk <= 16#9.6_3_5_5# ms;
  
  -- Multi-driven assignments
  bv <= "HL";
  kuoxdwmwq <= ('-', 'Z');
  mfao <= ('X', 'L');
end uhsaplhtp;



-- Seed after: 5005808920876086745,3108530264173481209
