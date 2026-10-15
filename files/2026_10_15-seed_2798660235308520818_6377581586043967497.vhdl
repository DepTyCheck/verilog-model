-- Seed: 2798660235308520818,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity dbo is
  port (qv : buffer std_logic_vector(0 downto 3));
end dbo;

architecture hp of dbo is
  
begin
  
end hp;

entity nodor is
  port (knvhl : out integer_vector(1 downto 2); oxyzag : buffer real);
end nodor;

library ieee;
use ieee.std_logic_1164.all;

architecture az of nodor is
  signal ofnianiop : std_logic_vector(0 downto 3);
  signal bq : std_logic_vector(0 downto 3);
begin
  vcrxnlsuzh : entity work.dbo
    port map (qv => bq);
  rnvt : entity work.dbo
    port map (qv => ofnianiop);
  z : entity work.dbo
    port map (qv => bq);
  
  -- Multi-driven assignments
  ofnianiop <= bq;
  bq <= (others => '0');
  bq <= "";
end az;

entity gkdhhhrtp is
  port (memzuvrr : out integer; jjzxdnooul : inout integer);
end gkdhhhrtp;

library ieee;
use ieee.std_logic_1164.all;

architecture gxj of gkdhhhrtp is
  signal ahhvgdxeg : std_logic_vector(0 downto 3);
begin
  yxcdhg : entity work.dbo
    port map (qv => ahhvgdxeg);
  
  -- Single-driven assignments
  jjzxdnooul <= 241;
  
  -- Multi-driven assignments
  ahhvgdxeg <= ahhvgdxeg;
  ahhvgdxeg <= ahhvgdxeg;
end gxj;

library ieee;
use ieee.std_logic_1164.all;

entity mzbvk is
  port (ydnvamr : out std_logic_vector(3 downto 3); llhn : buffer std_logic_vector(2 to 4));
end mzbvk;

architecture lgvskrepw of mzbvk is
  signal sonxl : real;
  signal dfg : integer_vector(1 downto 2);
  signal mby : real;
  signal kemuztxdto : integer_vector(1 downto 2);
begin
  skmkbsmi : entity work.nodor
    port map (knvhl => kemuztxdto, oxyzag => mby);
  oex : entity work.nodor
    port map (knvhl => dfg, oxyzag => sonxl);
  
  -- Multi-driven assignments
  llhn <= llhn;
  llhn <= "UXL";
end lgvskrepw;



-- Seed after: 13668953479249299313,6377581586043967497
