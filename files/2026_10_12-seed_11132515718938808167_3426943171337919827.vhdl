-- Seed: 11132515718938808167,3426943171337919827

entity bvnkxkzwea is
  port (pteq : buffer integer_vector(1 downto 4));
end bvnkxkzwea;

architecture yw of bvnkxkzwea is
  
begin
  -- Single-driven assignments
  pteq <= pteq;
end yw;

library ieee;
use ieee.std_logic_1164.all;

entity bkice is
  port (vzkjn : inout string(3 to 3); gauemxbjk : buffer std_logic; t : linkage time_vector(2 to 4); cqz : in integer);
end bkice;

architecture zcnhzdj of bkice is
  
begin
  -- Single-driven assignments
  vzkjn <= "m";
  
  -- Multi-driven assignments
  gauemxbjk <= gauemxbjk;
  gauemxbjk <= '-';
  gauemxbjk <= '1';
  gauemxbjk <= gauemxbjk;
end zcnhzdj;

entity do is
  port (xklczvlvf : inout bit_vector(0 to 2); fngmivt : out time_vector(1 downto 4); qfkcjs : in time);
end do;

architecture vus of do is
  signal etoksnqyt : integer_vector(1 downto 4);
  signal cv : integer_vector(1 downto 4);
  signal wlcgpvct : integer_vector(1 downto 4);
begin
  sapp : entity work.bvnkxkzwea
    port map (pteq => wlcgpvct);
  jydnd : entity work.bvnkxkzwea
    port map (pteq => cv);
  ekbgubstth : entity work.bvnkxkzwea
    port map (pteq => etoksnqyt);
  
  -- Single-driven assignments
  xklczvlvf <= ('1', '1', '1');
end vus;



-- Seed after: 16305079064226787783,3426943171337919827
