-- Seed: 5175273359183955550,15300320181035395489

library ieee;
use ieee.std_logic_1164.all;

entity wjyrzawn is
  port (annlomc : buffer time; llnbat : buffer boolean; elap : buffer severity_level; aed : linkage std_logic_vector(4 downto 3));
end wjyrzawn;

architecture posyb of wjyrzawn is
  
begin
  -- Single-driven assignments
  annlomc <= 16#A# us;
end posyb;

entity nqdumlaca is
  port (iztsxpmo : buffer real; ivwbkcjnxt : in integer);
end nqdumlaca;

library ieee;
use ieee.std_logic_1164.all;

architecture kejbox of nqdumlaca is
  signal odvuif : severity_level;
  signal dncwmbx : boolean;
  signal xdavedsi : time;
  signal xhndqibui : std_logic_vector(4 downto 3);
  signal sdj : severity_level;
  signal r : boolean;
  signal xc : time;
begin
  cbkr : entity work.wjyrzawn
    port map (annlomc => xc, llnbat => r, elap => sdj, aed => xhndqibui);
  jefyv : entity work.wjyrzawn
    port map (annlomc => xdavedsi, llnbat => dncwmbx, elap => odvuif, aed => xhndqibui);
  
  -- Multi-driven assignments
  xhndqibui <= "0H";
  xhndqibui <= ('X', 'X');
end kejbox;



-- Seed after: 10030231892831503189,15300320181035395489
