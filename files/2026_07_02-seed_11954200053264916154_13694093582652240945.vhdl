-- Seed: 11954200053264916154,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity ffstu is
  port (bhudbg : buffer std_logic_vector(0 to 2); t : out time_vector(0 downto 2); vnr : out boolean; mdzey : in std_logic_vector(2 to 3));
end ffstu;

architecture igyqxlzpf of ffstu is
  
begin
  -- Single-driven assignments
  vnr <= FALSE;
  t <= (others => 0 ns);
  
  -- Multi-driven assignments
  bhudbg <= ('U', 'W', 'L');
  bhudbg <= ('X', 'L', 'H');
end igyqxlzpf;

library ieee;
use ieee.std_logic_1164.all;

entity hjkty is
  port (bgk : inout std_logic; mmpp : out integer_vector(3 downto 4); tqdjftddj : in std_logic_vector(2 to 1); zolv : inout std_logic);
end hjkty;

library ieee;
use ieee.std_logic_1164.all;

architecture cnyeddozz of hjkty is
  signal fzfnqsvfvf : std_logic_vector(2 to 3);
  signal yv : boolean;
  signal luswqczxtw : time_vector(0 downto 2);
  signal ppkmhpf : boolean;
  signal oipkqdwufe : time_vector(0 downto 2);
  signal ygvdlaon : std_logic_vector(0 to 2);
  signal cdeiaefu : std_logic_vector(2 to 3);
  signal r : boolean;
  signal uqzerrg : time_vector(0 downto 2);
  signal enlcev : std_logic_vector(2 to 3);
  signal rxi : boolean;
  signal hdhxoboyx : time_vector(0 downto 2);
  signal cdc : std_logic_vector(0 to 2);
begin
  tj : entity work.ffstu
    port map (bhudbg => cdc, t => hdhxoboyx, vnr => rxi, mdzey => enlcev);
  dov : entity work.ffstu
    port map (bhudbg => cdc, t => uqzerrg, vnr => r, mdzey => cdeiaefu);
  uxnniskbu : entity work.ffstu
    port map (bhudbg => ygvdlaon, t => oipkqdwufe, vnr => ppkmhpf, mdzey => enlcev);
  mnbcjh : entity work.ffstu
    port map (bhudbg => cdc, t => luswqczxtw, vnr => yv, mdzey => fzfnqsvfvf);
  
  -- Single-driven assignments
  mmpp <= (others => 0);
  
  -- Multi-driven assignments
  enlcev <= "W0";
end cnyeddozz;



-- Seed after: 2152151759494231366,13694093582652240945
