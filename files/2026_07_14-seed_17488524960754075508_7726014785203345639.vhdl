-- Seed: 17488524960754075508,7726014785203345639

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity t is
  port ( asgyumad : inout integer_subtype_mirror
  ; asqxs : inout array_value_mirror
  ; hbfoea : buffer std_logic_vector(2 downto 2)
  ; oifr : inout severity_level
  );
end t;

architecture ln of t is
  
begin
  -- Multi-driven assignments
  hbfoea <= hbfoea;
  hbfoea <= "U";
  hbfoea <= hbfoea;
end ln;

use std.reflection.all;

entity u is
  port (sodj : in integer; zpegifewr : inout physical_value_mirror; q : inout real_vector(4 downto 2); wpj : inout character);
end u;

architecture bc of u is
  
begin
  -- Single-driven assignments
  wpj <= wpj;
  q <= q;
end bc;

use std.reflection.all;

entity qi is
  port (kqr : inout physical_value_mirror; azv : in bit);
end qi;

use std.reflection.all;

architecture egptw of qi is
  signal kpfnnus : character;
  signal ed : real_vector(4 downto 2);
  shared variable rjipxntn : physical_value_mirror;
  signal bl : integer;
begin
  sonereq : entity work.u
    port map (sodj => bl, zpegifewr => rjipxntn, q => ed, wpj => kpfnnus);
end egptw;

use std.reflection.all;

entity wiippsr is
  port (pnadto : inout enumeration_subtype_mirror);
end wiippsr;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture ws of wiippsr is
  signal ruiionlich : severity_level;
  signal gfwcq : std_logic_vector(2 downto 2);
  shared variable fqjfaq : array_value_mirror;
  shared variable jp : integer_subtype_mirror;
begin
  dcz : entity work.t
    port map (asgyumad => jp, asqxs => fqjfaq, hbfoea => gfwcq, oifr => ruiionlich);
  
  -- Multi-driven assignments
  gfwcq <= (others => 'U');
  gfwcq <= "H";
  gfwcq <= gfwcq;
  gfwcq <= "H";
end ws;



-- Seed after: 4877250678489846569,7726014785203345639
