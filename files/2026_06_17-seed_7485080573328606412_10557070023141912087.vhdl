-- Seed: 7485080573328606412,10557070023141912087

entity fchbximj is
  port (doqamw : buffer integer);
end fchbximj;

architecture l of fchbximj is
  
begin
  -- Single-driven assignments
  doqamw <= 2#1#;
end l;

entity pgzphn is
  port (iz : linkage real; sywle : inout real_vector(3 to 3));
end pgzphn;

architecture lcgcjb of pgzphn is
  signal bat : integer;
  signal egx : integer;
  signal zl : integer;
  signal plo : integer;
begin
  xlwlmceey : entity work.fchbximj
    port map (doqamw => plo);
  thzseoj : entity work.fchbximj
    port map (doqamw => zl);
  ajhf : entity work.fchbximj
    port map (doqamw => egx);
  bvxbr : entity work.fchbximj
    port map (doqamw => bat);
  
  -- Single-driven assignments
  sywle <= (others => 8#4.46#);
end lcgcjb;

entity phjsddchux is
  port (vopzudp : in time);
end phjsddchux;

architecture yyy of phjsddchux is
  signal kozo : integer;
  signal pl : integer;
  signal oti : integer;
begin
  yyjrlieah : entity work.fchbximj
    port map (doqamw => oti);
  na : entity work.fchbximj
    port map (doqamw => pl);
  lejro : entity work.fchbximj
    port map (doqamw => kozo);
end yyy;

library ieee;
use ieee.std_logic_1164.all;

entity sot is
  port (whlltdwa : in std_logic_vector(0 downto 0); owtns : out std_logic; lwsbuj : in integer);
end sot;

architecture gxeayb of sot is
  signal bo : integer;
  signal ui : integer;
  signal kk : time;
  signal fsaamb : real_vector(3 to 3);
  signal az : real;
begin
  sodel : entity work.pgzphn
    port map (iz => az, sywle => fsaamb);
  jkn : entity work.phjsddchux
    port map (vopzudp => kk);
  fouzk : entity work.fchbximj
    port map (doqamw => ui);
  zna : entity work.fchbximj
    port map (doqamw => bo);
  
  -- Single-driven assignments
  kk <= 3_1.3_0_1 fs;
  
  -- Multi-driven assignments
  owtns <= 'L';
  owtns <= '1';
end gxeayb;



-- Seed after: 15605556497234248457,10557070023141912087
