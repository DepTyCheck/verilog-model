-- Seed: 752786963409623526,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity aq is
  port (ndn : inout integer_subtype_mirror; csivhr : inout boolean; fk : linkage std_logic_vector(0 to 0));
end aq;

architecture ux of aq is
  
begin
  -- Single-driven assignments
  csivhr <= FALSE;
end ux;

use std.reflection.all;

entity l is
  port (hrvixik : inout access_subtype_mirror; fp : inout bit; vhgld : inout enumeration_value_mirror);
end l;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture mw of l is
  signal dht : std_logic_vector(0 to 0);
  signal baorqttdc : boolean;
  shared variable xohgqy : integer_subtype_mirror;
  signal cxcdcvuso : boolean;
  shared variable ywcojofmv : integer_subtype_mirror;
  signal us : boolean;
  shared variable w : integer_subtype_mirror;
  signal p : std_logic_vector(0 to 0);
  signal zfkxasnw : boolean;
  shared variable cxe : integer_subtype_mirror;
begin
  pykeppmm : entity work.aq
    port map (ndn => cxe, csivhr => zfkxasnw, fk => p);
  bz : entity work.aq
    port map (ndn => w, csivhr => us, fk => p);
  jzvetgblgv : entity work.aq
    port map (ndn => ywcojofmv, csivhr => cxcdcvuso, fk => p);
  bc : entity work.aq
    port map (ndn => xohgqy, csivhr => baorqttdc, fk => dht);
  
  -- Single-driven assignments
  fp <= fp;
  
  -- Multi-driven assignments
  p <= p;
end mw;



-- Seed after: 15910032061696538835,3566912872917928779
