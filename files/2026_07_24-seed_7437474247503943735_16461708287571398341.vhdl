-- Seed: 7437474247503943735,16461708287571398341

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (wnwk : buffer std_logic_vector(0 to 4); wwyp : buffer bit; np : in std_logic_vector(0 to 1));
end p;

architecture z of p is
  
begin
  -- Multi-driven assignments
  wnwk <= ('-', 'L', '0', '-', 'H');
end z;

library ieee;
use ieee.std_logic_1164.all;

entity hsxlgezs is
  port (chvwaibdai : linkage std_logic_vector(0 downto 3));
end hsxlgezs;

library ieee;
use ieee.std_logic_1164.all;

architecture zcz of hsxlgezs is
  signal zyintvxgg : bit;
  signal xzpol : std_logic_vector(0 to 4);
  signal ogfwp : bit;
  signal ryxcvtafmw : std_logic_vector(0 to 4);
  signal lytcyzm : std_logic_vector(0 to 1);
  signal tzspidgj : bit;
  signal vhbkjuqt : std_logic_vector(0 to 4);
begin
  ei : entity work.p
    port map (wnwk => vhbkjuqt, wwyp => tzspidgj, np => lytcyzm);
  igmosmmgy : entity work.p
    port map (wnwk => ryxcvtafmw, wwyp => ogfwp, np => lytcyzm);
  hhngebupyw : entity work.p
    port map (wnwk => xzpol, wwyp => zyintvxgg, np => lytcyzm);
  
  -- Multi-driven assignments
  vhbkjuqt <= vhbkjuqt;
  xzpol <= vhbkjuqt;
  vhbkjuqt <= vhbkjuqt;
end zcz;

entity brnbmmidpu is
  port (nmxjpws : out character);
end brnbmmidpu;

library ieee;
use ieee.std_logic_1164.all;

architecture owpuuqfkv of brnbmmidpu is
  signal eejxbvbc : bit;
  signal zsahvm : std_logic_vector(0 to 4);
  signal kophyutlf : std_logic_vector(0 to 1);
  signal o : bit;
  signal yprcly : std_logic_vector(0 to 4);
  signal x : std_logic_vector(0 to 1);
  signal jse : bit;
  signal pzddcbbfn : std_logic_vector(0 to 4);
begin
  hdhis : entity work.p
    port map (wnwk => pzddcbbfn, wwyp => jse, np => x);
  tshjfdlk : entity work.p
    port map (wnwk => yprcly, wwyp => o, np => kophyutlf);
  faj : entity work.p
    port map (wnwk => zsahvm, wwyp => eejxbvbc, np => kophyutlf);
  
  -- Single-driven assignments
  nmxjpws <= 'r';
end owpuuqfkv;



-- Seed after: 1504424700322721257,16461708287571398341
