-- Seed: 8080682966040884471,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity ff is
  port (f : out time_vector(4 downto 1); cafkr : in std_logic; lhurmefkzx : linkage std_logic_vector(3 downto 1));
end ff;

architecture hv of ff is
  
begin
  -- Single-driven assignments
  f <= (0 hr, 2#0_1_0_1_0# us, 0.4_2_0_3 ms, 16#3_7_3_E_A# fs);
end hv;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (canaw : inout std_logic; anpw : in integer);
end f;

library ieee;
use ieee.std_logic_1164.all;

architecture ffaaikcelb of f is
  signal hzwxhka : std_logic_vector(3 downto 1);
  signal lhtdsqs : std_logic;
  signal svchesh : time_vector(4 downto 1);
  signal pmjn : std_logic_vector(3 downto 1);
  signal mglvyahm : time_vector(4 downto 1);
  signal wmgbeculej : std_logic_vector(3 downto 1);
  signal eddykmypq : time_vector(4 downto 1);
  signal eldazjmxul : std_logic_vector(3 downto 1);
  signal xl : time_vector(4 downto 1);
begin
  o : entity work.ff
    port map (f => xl, cafkr => canaw, lhurmefkzx => eldazjmxul);
  dccrit : entity work.ff
    port map (f => eddykmypq, cafkr => canaw, lhurmefkzx => wmgbeculej);
  zv : entity work.ff
    port map (f => mglvyahm, cafkr => canaw, lhurmefkzx => pmjn);
  otjxwxfs : entity work.ff
    port map (f => svchesh, cafkr => lhtdsqs, lhurmefkzx => hzwxhka);
  
  -- Multi-driven assignments
  pmjn <= ('0', 'H', 'U');
  wmgbeculej <= ('-', 'H', 'H');
  eldazjmxul <= ('0', '0', 'L');
end ffaaikcelb;



-- Seed after: 4737862834545015222,15300320181035395489
