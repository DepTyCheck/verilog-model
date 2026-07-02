-- Seed: 1181707482615652638,14426950258250697445

use std.reflection.all;

entity azjnw is
  port (ovntgj : inout physical_value_mirror; tz : inout floating_value_mirror);
end azjnw;

architecture urmbmz of azjnw is
  
begin
  
end urmbmz;

library ieee;
use ieee.std_logic_1164.all;

entity knjxfkv is
  port (gnc : out std_logic_vector(2 downto 4));
end knjxfkv;

use std.reflection.all;

architecture ochetzqogn of knjxfkv is
  shared variable zwy : floating_value_mirror;
  shared variable duiecqmfm : physical_value_mirror;
  shared variable vrknwds : floating_value_mirror;
  shared variable urhwwuflwp : physical_value_mirror;
  shared variable wyzvxnfs : floating_value_mirror;
  shared variable w : physical_value_mirror;
begin
  z : entity work.azjnw
    port map (ovntgj => w, tz => wyzvxnfs);
  p : entity work.azjnw
    port map (ovntgj => urhwwuflwp, tz => vrknwds);
  emhrp : entity work.azjnw
    port map (ovntgj => duiecqmfm, tz => zwy);
  
  -- Multi-driven assignments
  gnc <= (others => '0');
  gnc <= gnc;
  gnc <= gnc;
  gnc <= (others => '0');
end ochetzqogn;



-- Seed after: 15593355501411692632,14426950258250697445
