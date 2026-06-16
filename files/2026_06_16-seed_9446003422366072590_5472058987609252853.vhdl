-- Seed: 9446003422366072590,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity phl is
  port (xphf : buffer std_logic_vector(3 to 4); pnp : buffer real; dsrwgk : out time_vector(1 downto 1); ykvhm : linkage std_logic_vector(1 downto 3));
end phl;

architecture arxqb of phl is
  
begin
  -- Single-driven assignments
  dsrwgk <= (others => 8#45.7# fs);
  pnp <= 16#A7963.1_3_4_B#;
  
  -- Multi-driven assignments
  xphf <= "ZX";
  xphf <= ('H', 'U');
  xphf <= "HH";
end arxqb;

library ieee;
use ieee.std_logic_1164.all;

entity tobe is
  port (jkfh : inout severity_level; pkqgqfaot : out integer; ibglphmjmi : buffer std_logic; pawnex : buffer real);
end tobe;

library ieee;
use ieee.std_logic_1164.all;

architecture xgtxldamlt of tobe is
  signal wvkpozx : time_vector(1 downto 1);
  signal b : std_logic_vector(1 downto 3);
  signal epdftuey : time_vector(1 downto 1);
  signal saibxlr : real;
  signal eqhnenx : time_vector(1 downto 1);
  signal asiqjmu : real;
  signal qopwf : std_logic_vector(1 downto 3);
  signal tiaxqkej : time_vector(1 downto 1);
  signal yysn : real;
  signal hv : std_logic_vector(3 to 4);
begin
  zdfgad : entity work.phl
    port map (xphf => hv, pnp => yysn, dsrwgk => tiaxqkej, ykvhm => qopwf);
  tyw : entity work.phl
    port map (xphf => hv, pnp => asiqjmu, dsrwgk => eqhnenx, ykvhm => qopwf);
  dmpvxchgo : entity work.phl
    port map (xphf => hv, pnp => saibxlr, dsrwgk => epdftuey, ykvhm => b);
  rbiisbd : entity work.phl
    port map (xphf => hv, pnp => pawnex, dsrwgk => wvkpozx, ykvhm => qopwf);
  
  -- Single-driven assignments
  pkqgqfaot <= 421;
  jkfh <= ERROR;
  
  -- Multi-driven assignments
  ibglphmjmi <= '1';
  b <= (others => '0');
  ibglphmjmi <= 'X';
  qopwf <= (others => '0');
end xgtxldamlt;

library ieee;
use ieee.std_logic_1164.all;

entity kjigdamck is
  port (nxtbnpns : inout time_vector(0 to 0); hyu : inout severity_level; ifyql : in std_logic; yy : out std_logic);
end kjigdamck;

library ieee;
use ieee.std_logic_1164.all;

architecture ihwf of kjigdamck is
  signal no : real;
  signal bzhzgrbwm : std_logic;
  signal ttfepmy : integer;
begin
  hncysmlktr : entity work.tobe
    port map (jkfh => hyu, pkqgqfaot => ttfepmy, ibglphmjmi => bzhzgrbwm, pawnex => no);
  
  -- Single-driven assignments
  nxtbnpns <= (others => 2#0000.0_0# us);
  
  -- Multi-driven assignments
  yy <= '1';
  yy <= 'Z';
end ihwf;



-- Seed after: 11701228341787848520,5472058987609252853
