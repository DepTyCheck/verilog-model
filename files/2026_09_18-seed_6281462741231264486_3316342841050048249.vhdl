-- Seed: 6281462741231264486,3316342841050048249

library ieee;
use ieee.std_logic_1164.all;

entity imv is
  port (ilgkdbqag : out std_logic_vector(0 downto 4); vtljha : buffer std_logic; cfrs : inout real; qjrzjvata : out integer);
end imv;

architecture wnhsg of imv is
  
begin
  -- Single-driven assignments
  cfrs <= 4323.21;
  qjrzjvata <= 2#0#;
  
  -- Multi-driven assignments
  vtljha <= '0';
  vtljha <= vtljha;
end wnhsg;

entity h is
  port (ozqdznbir : in time; euvmq : buffer integer);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture xspvpn of h is
  signal mof : real;
  signal uviiofew : std_logic;
  signal durakdxcjw : integer;
  signal kihr : real;
  signal vjg : std_logic;
  signal nxm : std_logic_vector(0 downto 4);
  signal xmeqls : integer;
  signal hzbh : real;
  signal vkrf : std_logic;
  signal vmgavifch : std_logic_vector(0 downto 4);
begin
  opobkjxn : entity work.imv
    port map (ilgkdbqag => vmgavifch, vtljha => vkrf, cfrs => hzbh, qjrzjvata => xmeqls);
  em : entity work.imv
    port map (ilgkdbqag => nxm, vtljha => vjg, cfrs => kihr, qjrzjvata => durakdxcjw);
  kbycjbzs : entity work.imv
    port map (ilgkdbqag => vmgavifch, vtljha => uviiofew, cfrs => mof, qjrzjvata => euvmq);
  
  -- Multi-driven assignments
  vmgavifch <= vmgavifch;
  vjg <= 'H';
  vmgavifch <= "";
  vkrf <= vkrf;
end xspvpn;



-- Seed after: 16383751054977145609,3316342841050048249
