-- Seed: 6789582743391677675,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity xqkcwsg is
  port (kzpitva : linkage std_logic_vector(0 downto 3); g : in time; soxonzqpy : inout integer; tgecmqrlbz : linkage std_logic_vector(2 to 1));
end xqkcwsg;

architecture apdwianh of xqkcwsg is
  
begin
  -- Single-driven assignments
  soxonzqpy <= 0_4_1;
end apdwianh;

entity xlf is
  port (zxk : inout real);
end xlf;

library ieee;
use ieee.std_logic_1164.all;

architecture kke of xlf is
  signal oh : std_logic_vector(2 to 1);
  signal ealklmfde : integer;
  signal rebmwdli : std_logic_vector(0 downto 3);
  signal qvlsp : std_logic_vector(2 to 1);
  signal qlsbz : integer;
  signal noay : time;
  signal ukmkhoauv : std_logic_vector(0 downto 3);
  signal bsihv : integer;
  signal cf : time;
  signal qdl : std_logic_vector(0 downto 3);
begin
  mgnyqn : entity work.xqkcwsg
    port map (kzpitva => qdl, g => cf, soxonzqpy => bsihv, tgecmqrlbz => ukmkhoauv);
  ogq : entity work.xqkcwsg
    port map (kzpitva => ukmkhoauv, g => noay, soxonzqpy => qlsbz, tgecmqrlbz => qvlsp);
  afy : entity work.xqkcwsg
    port map (kzpitva => rebmwdli, g => noay, soxonzqpy => ealklmfde, tgecmqrlbz => oh);
  
  -- Single-driven assignments
  zxk <= 16#6_C_2.7E014#;
  noay <= noay;
  cf <= cf;
  
  -- Multi-driven assignments
  rebmwdli <= rebmwdli;
end kke;

entity tuifkcl is
  port (cw : buffer integer; ecwz : out time; qu : buffer real);
end tuifkcl;

library ieee;
use ieee.std_logic_1164.all;

architecture tujeqv of tuifkcl is
  signal xlhfvwl : time;
  signal xyvdx : integer;
  signal egvetfd : time;
  signal h : std_logic_vector(2 to 1);
begin
  btjku : entity work.xqkcwsg
    port map (kzpitva => h, g => egvetfd, soxonzqpy => xyvdx, tgecmqrlbz => h);
  gcqhzuxfon : entity work.xqkcwsg
    port map (kzpitva => h, g => xlhfvwl, soxonzqpy => cw, tgecmqrlbz => h);
  
  -- Multi-driven assignments
  h <= h;
end tujeqv;

entity rpldqwnuvg is
  port (st : out real);
end rpldqwnuvg;

architecture qriwxs of rpldqwnuvg is
  
begin
  
end qriwxs;



-- Seed after: 16329955596225910126,17234720251424330329
