-- Seed: 16846456281359205803,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity isvpqeiu is
  port (livlg : out std_logic_vector(1 to 2); slvdssl : inout std_logic_vector(3 downto 0); lm : out time);
end isvpqeiu;

architecture zthkgn of isvpqeiu is
  
begin
  -- Multi-driven assignments
  livlg <= ('H', 'X');
  livlg <= ('X', 'X');
  slvdssl <= "WL1L";
  livlg <= ('X', 'X');
end zthkgn;

library ieee;
use ieee.std_logic_1164.all;

entity qtjl is
  port (kzksi : in integer; xbrlceadf : in std_logic_vector(0 downto 1));
end qtjl;

library ieee;
use ieee.std_logic_1164.all;

architecture avyxuzgpp of qtjl is
  signal nab : time;
  signal wnliarv : std_logic_vector(3 downto 0);
  signal w : std_logic_vector(1 to 2);
  signal esozn : time;
  signal m : std_logic_vector(1 to 2);
  signal rt : time;
  signal wfzupqn : std_logic_vector(3 downto 0);
  signal jby : std_logic_vector(1 to 2);
begin
  c : entity work.isvpqeiu
    port map (livlg => jby, slvdssl => wfzupqn, lm => rt);
  pvkx : entity work.isvpqeiu
    port map (livlg => m, slvdssl => wfzupqn, lm => esozn);
  pydxqztafz : entity work.isvpqeiu
    port map (livlg => w, slvdssl => wnliarv, lm => nab);
  
  -- Multi-driven assignments
  wfzupqn <= "--U0";
  wfzupqn <= "WLWZ";
end avyxuzgpp;



-- Seed after: 9567579380389778318,8118127366649987907
