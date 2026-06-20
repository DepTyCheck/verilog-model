-- Seed: 3766616083603853050,17924494779688682807

entity bwjdnezf is
  port (lxmod : in real_vector(2 downto 3); vtu : out time_vector(2 downto 2));
end bwjdnezf;

architecture ntuqlg of bwjdnezf is
  
begin
  -- Single-driven assignments
  vtu <= (others => 1004 us);
end ntuqlg;

entity ojv is
  port (ag : buffer time);
end ojv;

architecture thagnkidkx of ojv is
  signal gsyreixyle : time_vector(2 downto 2);
  signal ilqcjx : time_vector(2 downto 2);
  signal nisaxlxfwp : time_vector(2 downto 2);
  signal rlbvq : time_vector(2 downto 2);
  signal gvegpl : real_vector(2 downto 3);
begin
  tocn : entity work.bwjdnezf
    port map (lxmod => gvegpl, vtu => rlbvq);
  v : entity work.bwjdnezf
    port map (lxmod => gvegpl, vtu => nisaxlxfwp);
  i : entity work.bwjdnezf
    port map (lxmod => gvegpl, vtu => ilqcjx);
  suumagfwks : entity work.bwjdnezf
    port map (lxmod => gvegpl, vtu => gsyreixyle);
end thagnkidkx;

library ieee;
use ieee.std_logic_1164.all;

entity aimkz is
  port (rwxcgdqngl : linkage real; kctgohte : linkage std_logic_vector(0 to 3); ktymclj : buffer std_logic_vector(2 to 3));
end aimkz;

architecture idld of aimkz is
  
begin
  -- Multi-driven assignments
  ktymclj <= ('L', 'Z');
end idld;



-- Seed after: 2162418475203702493,17924494779688682807
