-- Seed: 1552283497271386304,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (ivpu : linkage real; x : buffer std_logic_vector(4 downto 1); vedmfaec : linkage time; l : inout integer);
end v;

architecture zmmexubo of v is
  
begin
  -- Single-driven assignments
  l <= 0_4_2_0;
  
  -- Multi-driven assignments
  x <= "XZWL";
  x <= "WXL1";
  x <= x;
end zmmexubo;

entity i is
  port (kpjrsmf : linkage character; d : inout time; pxzwwpre : buffer real);
end i;

library ieee;
use ieee.std_logic_1164.all;

architecture ccdlmh of i is
  signal vxboxppw : integer;
  signal uq : std_logic_vector(4 downto 1);
  signal dtjg : integer;
  signal lb : time;
  signal gojp : std_logic_vector(4 downto 1);
  signal p : real;
  signal wbudmjuv : integer;
  signal uckqjf : time;
  signal nzcfawl : std_logic_vector(4 downto 1);
  signal osugvv : real;
begin
  hvefq : entity work.v
    port map (ivpu => osugvv, x => nzcfawl, vedmfaec => uckqjf, l => wbudmjuv);
  sswuw : entity work.v
    port map (ivpu => p, x => gojp, vedmfaec => lb, l => dtjg);
  wtrh : entity work.v
    port map (ivpu => pxzwwpre, x => uq, vedmfaec => d, l => vxboxppw);
  
  -- Multi-driven assignments
  gojp <= nzcfawl;
  nzcfawl <= ('-', 'U', 'X', '-');
end ccdlmh;



-- Seed after: 3918200142981025618,1112937151005418631
