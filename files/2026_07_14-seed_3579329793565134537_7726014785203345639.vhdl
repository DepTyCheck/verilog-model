-- Seed: 3579329793565134537,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (so : inout integer; tctx : linkage std_logic; zthmtleyff : buffer std_logic);
end x;

architecture rnxdhkenw of x is
  
begin
  -- Single-driven assignments
  so <= so;
  
  -- Multi-driven assignments
  zthmtleyff <= 'L';
  zthmtleyff <= zthmtleyff;
  zthmtleyff <= 'X';
end rnxdhkenw;

use std.reflection.all;

entity bxyaw is
  port (r : out bit; cdksigmr : inout record_subtype_mirror; pfel : inout access_subtype_mirror);
end bxyaw;

library ieee;
use ieee.std_logic_1164.all;

architecture k of bxyaw is
  signal ut : integer;
  signal ho : std_logic;
  signal jnmtfhuv : integer;
begin
  smvyntn : entity work.x
    port map (so => jnmtfhuv, tctx => ho, zthmtleyff => ho);
  g : entity work.x
    port map (so => ut, tctx => ho, zthmtleyff => ho);
  
  -- Single-driven assignments
  r <= r;
  
  -- Multi-driven assignments
  ho <= ho;
end k;



-- Seed after: 15759688724878350291,7726014785203345639
