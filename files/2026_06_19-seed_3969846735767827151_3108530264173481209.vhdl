-- Seed: 3969846735767827151,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity kkrsn is
  port (rudg : out real; yfn : in std_logic);
end kkrsn;

architecture zvlvmk of kkrsn is
  
begin
  -- Single-driven assignments
  rudg <= 2#0.0_1_1#;
end zvlvmk;

library ieee;
use ieee.std_logic_1164.all;

entity dsiro is
  port (euqdllhcq : buffer integer; j : in std_logic_vector(4 to 3); udzaoqnayk : buffer real; wns : buffer std_logic_vector(0 downto 4));
end dsiro;

architecture cxzw of dsiro is
  
begin
  -- Multi-driven assignments
  wns <= (others => '0');
  wns <= (others => '0');
end cxzw;

library ieee;
use ieee.std_logic_1164.all;

entity neindi is
  port (wnrcxys : buffer real; dvvasg : inout real; mikxseizxf : in std_logic; usucfmh : in real);
end neindi;

architecture b of neindi is
  
begin
  -- Single-driven assignments
  dvvasg <= 4_3_3_0.32;
  wnrcxys <= 2#000.100#;
end b;

entity zhcdl is
  port (qp : inout integer; ofehzvbw : in real);
end zhcdl;

library ieee;
use ieee.std_logic_1164.all;

architecture yxojcbjwq of zhcdl is
  signal rz : std_logic;
  signal yyouv : real;
  signal kbe : real;
  signal vc : std_logic;
  signal bgultpsm : real;
  signal ijnlbozfnv : real;
  signal ognryzk : real;
  signal tldvdtxjp : std_logic_vector(0 downto 4);
begin
  tjfehq : entity work.dsiro
    port map (euqdllhcq => qp, j => tldvdtxjp, udzaoqnayk => ognryzk, wns => tldvdtxjp);
  pwljbdnowy : entity work.neindi
    port map (wnrcxys => ijnlbozfnv, dvvasg => bgultpsm, mikxseizxf => vc, usucfmh => kbe);
  ldj : entity work.kkrsn
    port map (rudg => yyouv, yfn => rz);
  
  -- Single-driven assignments
  kbe <= 2#1_1_1_1_0.0#;
  
  -- Multi-driven assignments
  tldvdtxjp <= "";
  rz <= 'H';
end yxojcbjwq;



-- Seed after: 18126331364637760292,3108530264173481209
