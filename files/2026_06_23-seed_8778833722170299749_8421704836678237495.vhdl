-- Seed: 8778833722170299749,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity ofi is
  port (nw : in std_logic_vector(1 downto 2); a : in integer);
end ofi;

architecture klousrq of ofi is
  
begin
  
end klousrq;

entity trazpsiz is
  port (tgwj : linkage severity_level);
end trazpsiz;

library ieee;
use ieee.std_logic_1164.all;

architecture pf of trazpsiz is
  signal mwspqbrk : integer;
  signal o : integer;
  signal iecljy : integer;
  signal s : std_logic_vector(1 downto 2);
  signal hchbs : integer;
  signal i : std_logic_vector(1 downto 2);
begin
  vbqcuwgeez : entity work.ofi
    port map (nw => i, a => hchbs);
  htjlm : entity work.ofi
    port map (nw => s, a => iecljy);
  yxgidycfv : entity work.ofi
    port map (nw => i, a => o);
  tez : entity work.ofi
    port map (nw => i, a => mwspqbrk);
  
  -- Single-driven assignments
  hchbs <= 11211;
  o <= 2#0_1_1_0#;
  mwspqbrk <= 3_3_0_0;
  iecljy <= 421;
  
  -- Multi-driven assignments
  i <= "";
  s <= (others => '0');
  i <= (others => '0');
  s <= (others => '0');
end pf;

entity pshp is
  port (xnkugyeqi : inout severity_level; tahqyercvs : buffer real);
end pshp;

library ieee;
use ieee.std_logic_1164.all;

architecture tqtccyst of pshp is
  signal sirpll : integer;
  signal wwgfvawqrn : std_logic_vector(1 downto 2);
begin
  daszly : entity work.ofi
    port map (nw => wwgfvawqrn, a => sirpll);
  
  -- Single-driven assignments
  tahqyercvs <= 16#E_C_B.C#;
  xnkugyeqi <= WARNING;
  
  -- Multi-driven assignments
  wwgfvawqrn <= "";
end tqtccyst;



-- Seed after: 1782962434098358795,8421704836678237495
