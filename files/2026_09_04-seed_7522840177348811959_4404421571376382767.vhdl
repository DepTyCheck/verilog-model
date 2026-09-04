-- Seed: 7522840177348811959,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity axxypyix is
  port (chehhzwm : in std_logic_vector(2 downto 1); ystrf : buffer std_logic; xpvymgxqaw : out integer; lgere : in std_logic_vector(3 downto 4));
end axxypyix;

architecture lkvpzgpvhp of axxypyix is
  
begin
  -- Single-driven assignments
  xpvymgxqaw <= 3234;
  
  -- Multi-driven assignments
  ystrf <= ystrf;
  ystrf <= 'U';
end lkvpzgpvhp;

library ieee;
use ieee.std_logic_1164.all;

entity widho is
  port (ljzoxoeuif : buffer std_logic);
end widho;

library ieee;
use ieee.std_logic_1164.all;

architecture cjmjwgt of widho is
  signal w : std_logic_vector(3 downto 4);
  signal vmej : integer;
  signal vznbqge : std_logic_vector(2 downto 1);
begin
  q : entity work.axxypyix
    port map (chehhzwm => vznbqge, ystrf => ljzoxoeuif, xpvymgxqaw => vmej, lgere => w);
end cjmjwgt;

entity lpcrwxolx is
  port (khrinow : inout real);
end lpcrwxolx;

library ieee;
use ieee.std_logic_1164.all;

architecture dp of lpcrwxolx is
  signal wlnvbtwboj : std_logic;
begin
  prn : entity work.widho
    port map (ljzoxoeuif => wlnvbtwboj);
  
  -- Single-driven assignments
  khrinow <= khrinow;
  
  -- Multi-driven assignments
  wlnvbtwboj <= 'L';
  wlnvbtwboj <= 'Z';
end dp;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (pdnoh : linkage std_logic_vector(1 downto 4));
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture va of m is
  signal hbzw : std_logic;
  signal gur : std_logic_vector(3 downto 4);
  signal bij : integer;
  signal tr : std_logic_vector(2 downto 1);
  signal vyuyntpyw : std_logic;
  signal tglpad : real;
begin
  ithzp : entity work.lpcrwxolx
    port map (khrinow => tglpad);
  dsblpgyg : entity work.widho
    port map (ljzoxoeuif => vyuyntpyw);
  uccoee : entity work.axxypyix
    port map (chehhzwm => tr, ystrf => vyuyntpyw, xpvymgxqaw => bij, lgere => gur);
  jmauxqqy : entity work.widho
    port map (ljzoxoeuif => hbzw);
  
  -- Multi-driven assignments
  hbzw <= vyuyntpyw;
  vyuyntpyw <= '0';
end va;



-- Seed after: 7901122350902629068,4404421571376382767
