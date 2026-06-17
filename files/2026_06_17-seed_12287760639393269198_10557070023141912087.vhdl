-- Seed: 12287760639393269198,10557070023141912087

entity wjnzfkwu is
  port (lwhbghou : out time);
end wjnzfkwu;

architecture p of wjnzfkwu is
  
begin
  -- Single-driven assignments
  lwhbghou <= 1 hr;
end p;

library ieee;
use ieee.std_logic_1164.all;

entity uetvkwl is
  port (fjyxlb : buffer std_logic; bq : in bit);
end uetvkwl;

architecture ojpgcyf of uetvkwl is
  signal tjrlcaorpi : time;
begin
  tzspfbcqg : entity work.wjnzfkwu
    port map (lwhbghou => tjrlcaorpi);
  
  -- Multi-driven assignments
  fjyxlb <= '-';
  fjyxlb <= '-';
  fjyxlb <= 'H';
end ojpgcyf;

library ieee;
use ieee.std_logic_1164.all;

entity lz is
  port (ikyexso : in time; nsysiwcnut : in integer_vector(1 to 3); benckaz : in integer; bq : in std_logic_vector(2 to 4));
end lz;

library ieee;
use ieee.std_logic_1164.all;

architecture b of lz is
  signal s : time;
  signal hzaqmendb : bit;
  signal bwlnh : std_logic;
  signal zzykxrzqv : time;
begin
  guevk : entity work.wjnzfkwu
    port map (lwhbghou => zzykxrzqv);
  yz : entity work.uetvkwl
    port map (fjyxlb => bwlnh, bq => hzaqmendb);
  ctwymlz : entity work.wjnzfkwu
    port map (lwhbghou => s);
  
  -- Multi-driven assignments
  bwlnh <= 'Z';
  bwlnh <= 'U';
end b;



-- Seed after: 7795623162439630675,10557070023141912087
