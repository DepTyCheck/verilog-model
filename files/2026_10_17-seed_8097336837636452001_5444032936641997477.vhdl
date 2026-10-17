-- Seed: 8097336837636452001,5444032936641997477

library ieee;
use ieee.std_logic_1164.all;

entity kruakypdbx is
  port (xhnex : buffer std_logic_vector(1 to 1));
end kruakypdbx;

architecture rrh of kruakypdbx is
  
begin
  -- Multi-driven assignments
  xhnex <= "X";
  xhnex <= xhnex;
  xhnex <= "L";
end rrh;

entity cleieovg is
  port (buprq : buffer integer; xcrttxbl : inout integer);
end cleieovg;

library ieee;
use ieee.std_logic_1164.all;

architecture mvgruy of cleieovg is
  signal zbzltfyt : std_logic_vector(1 to 1);
begin
  vyyzc : entity work.kruakypdbx
    port map (xhnex => zbzltfyt);
  jljax : entity work.kruakypdbx
    port map (xhnex => zbzltfyt);
  hesxiggygm : entity work.kruakypdbx
    port map (xhnex => zbzltfyt);
  
  -- Single-driven assignments
  buprq <= xcrttxbl;
end mvgruy;

library ieee;
use ieee.std_logic_1164.all;

entity ayhgekia is
  port (r : out std_logic_vector(0 downto 2); j : in std_logic; tabsejfx : out boolean_vector(1 downto 1));
end ayhgekia;

library ieee;
use ieee.std_logic_1164.all;

architecture i of ayhgekia is
  signal auhqq : integer;
  signal bmefsyfag : integer;
  signal satnzfnadh : std_logic_vector(1 to 1);
begin
  zwzvrd : entity work.kruakypdbx
    port map (xhnex => satnzfnadh);
  rzfocadsp : entity work.cleieovg
    port map (buprq => bmefsyfag, xcrttxbl => auhqq);
  
  -- Single-driven assignments
  tabsejfx <= (others => TRUE);
end i;

library ieee;
use ieee.std_logic_1164.all;

entity ujq is
  port (v : in bit_vector(3 downto 2); vw : linkage real; yxovoidz : inout std_logic; cnocev : buffer std_logic_vector(2 to 2));
end ujq;

library ieee;
use ieee.std_logic_1164.all;

architecture mbucisdjag of ujq is
  signal kkcjq : boolean_vector(1 downto 1);
  signal byuvay : boolean_vector(1 downto 1);
  signal zozedpkz : std_logic;
  signal nolxyy : std_logic_vector(0 downto 2);
begin
  moitgjmsr : entity work.ayhgekia
    port map (r => nolxyy, j => zozedpkz, tabsejfx => byuvay);
  hnqyxmmh : entity work.ayhgekia
    port map (r => nolxyy, j => yxovoidz, tabsejfx => kkcjq);
  xeqqjd : entity work.kruakypdbx
    port map (xhnex => cnocev);
  
  -- Multi-driven assignments
  nolxyy <= (others => '0');
  zozedpkz <= yxovoidz;
  cnocev <= (others => 'X');
  cnocev <= "-";
end mbucisdjag;



-- Seed after: 3599447146657709198,5444032936641997477
