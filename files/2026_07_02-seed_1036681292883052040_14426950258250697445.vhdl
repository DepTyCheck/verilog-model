-- Seed: 1036681292883052040,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (h : inout std_logic_vector(3 to 1); k : inout real);
end z;

architecture vq of z is
  
begin
  -- Single-driven assignments
  k <= 1410.330;
end vq;

use std.reflection.all;

entity sjy is
  port (gbdk : inout record_value_mirror; yq : inout subtype_mirror);
end sjy;

library ieee;
use ieee.std_logic_1164.all;

architecture rdsmnrgz of sjy is
  signal muofjaomv : real;
  signal fcnivepc : real;
  signal q : std_logic_vector(3 to 1);
begin
  sb : entity work.z
    port map (h => q, k => fcnivepc);
  oyxcnlcy : entity work.z
    port map (h => q, k => muofjaomv);
  
  -- Multi-driven assignments
  q <= (others => '0');
  q <= q;
end rdsmnrgz;

use std.reflection.all;

entity gxnu is
  port ( mvcbpyr : inout physical_value_mirror
  ; vxdxfxno : linkage bit_vector(3 to 4)
  ; njhjr : linkage bit_vector(3 to 0)
  ; atzsh : inout array_value_mirror
  );
end gxnu;

library ieee;
use ieee.std_logic_1164.all;

architecture ubgvpnwt of gxnu is
  signal kjfrosusk : real;
  signal kum : std_logic_vector(3 to 1);
begin
  mhjxk : entity work.z
    port map (h => kum, k => kjfrosusk);
end ubgvpnwt;

use std.reflection.all;

entity ehwersbao is
  port (sogw : in real; pamgaqss : inout access_subtype_mirror; wpkknyqs : inout bit);
end ehwersbao;

library ieee;
use ieee.std_logic_1164.all;

architecture cafxwp of ehwersbao is
  signal jobeo : real;
  signal r : std_logic_vector(3 to 1);
begin
  soydvjzx : entity work.z
    port map (h => r, k => jobeo);
  
  -- Single-driven assignments
  wpkknyqs <= wpkknyqs;
  
  -- Multi-driven assignments
  r <= (others => '0');
end cafxwp;



-- Seed after: 2183106194193807854,14426950258250697445
