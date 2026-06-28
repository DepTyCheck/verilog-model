-- Seed: 15901147232646747301,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity njirzu is
  port (bqicnk : inout bit_vector(1 downto 0); li : in std_logic_vector(3 downto 0); w : buffer real);
end njirzu;

architecture mnodoylmg of njirzu is
  
begin
  -- Single-driven assignments
  bqicnk <= ('1', '0');
  w <= 16#6.4_6#;
end mnodoylmg;

library ieee;
use ieee.std_logic_1164.all;

entity njr is
  port (fad : buffer integer; kkwwde : in std_logic; reh : in real; ichowjpn : buffer integer);
end njr;

library ieee;
use ieee.std_logic_1164.all;

architecture vq of njr is
  signal rpwo : real;
  signal fu : bit_vector(1 downto 0);
  signal htla : real;
  signal bs : std_logic_vector(3 downto 0);
  signal a : bit_vector(1 downto 0);
begin
  uguplhcqzh : entity work.njirzu
    port map (bqicnk => a, li => bs, w => htla);
  xtm : entity work.njirzu
    port map (bqicnk => fu, li => bs, w => rpwo);
  
  -- Single-driven assignments
  ichowjpn <= 4_4;
  fad <= 3;
  
  -- Multi-driven assignments
  bs <= "XLZL";
  bs <= ('-', '-', '0', 'U');
  bs <= ('W', 'W', 'Z', 'W');
  bs <= "Z-L0";
end vq;

entity qamotvl is
  port (h : inout boolean);
end qamotvl;

library ieee;
use ieee.std_logic_1164.all;

architecture wzndeeispm of qamotvl is
  signal bdzluc : real;
  signal noetsyzz : bit_vector(1 downto 0);
  signal mmhmln : std_logic_vector(3 downto 0);
  signal hzhsharo : bit_vector(1 downto 0);
  signal u : integer;
  signal wdwyc : std_logic;
  signal wx : integer;
  signal xxurl : integer;
  signal kpog : real;
  signal mplcshpr : std_logic;
  signal shdpro : integer;
begin
  ghpdq : entity work.njr
    port map (fad => shdpro, kkwwde => mplcshpr, reh => kpog, ichowjpn => xxurl);
  odr : entity work.njr
    port map (fad => wx, kkwwde => wdwyc, reh => kpog, ichowjpn => u);
  w : entity work.njirzu
    port map (bqicnk => hzhsharo, li => mmhmln, w => kpog);
  pvhpyuvagc : entity work.njirzu
    port map (bqicnk => noetsyzz, li => mmhmln, w => bdzluc);
  
  -- Single-driven assignments
  h <= FALSE;
  
  -- Multi-driven assignments
  mmhmln <= ('H', 'Z', '-', 'H');
  wdwyc <= 'U';
  mplcshpr <= 'U';
end wzndeeispm;

entity knk is
  port (ieobkbma : buffer boolean);
end knk;

library ieee;
use ieee.std_logic_1164.all;

architecture gyonvjmo of knk is
  signal q : boolean;
  signal tmzzip : integer;
  signal hrjdd : std_logic;
  signal mxs : integer;
  signal jegi : real;
  signal ldsvyefyu : bit_vector(1 downto 0);
  signal bldhinf : real;
  signal qlpysknd : std_logic_vector(3 downto 0);
  signal gaxfvty : bit_vector(1 downto 0);
begin
  uflm : entity work.njirzu
    port map (bqicnk => gaxfvty, li => qlpysknd, w => bldhinf);
  lnulrvss : entity work.njirzu
    port map (bqicnk => ldsvyefyu, li => qlpysknd, w => jegi);
  eanjbswj : entity work.njr
    port map (fad => mxs, kkwwde => hrjdd, reh => bldhinf, ichowjpn => tmzzip);
  zbil : entity work.qamotvl
    port map (h => q);
  
  -- Single-driven assignments
  ieobkbma <= FALSE;
  
  -- Multi-driven assignments
  qlpysknd <= ('X', 'Z', 'Z', 'Z');
  qlpysknd <= "HHHU";
end gyonvjmo;



-- Seed after: 3914320423881206527,6697892553037813751
