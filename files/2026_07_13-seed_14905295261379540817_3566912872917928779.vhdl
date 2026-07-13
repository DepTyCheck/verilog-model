-- Seed: 14905295261379540817,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity dgcwj is
  port (xpn : in std_logic);
end dgcwj;

architecture ovvwcfc of dgcwj is
  
begin
  
end ovvwcfc;

use std.reflection.all;

entity kg is
  port (mwcbyc : inout physical_value_mirror; aoelf : inout real);
end kg;

library ieee;
use ieee.std_logic_1164.all;

architecture yzve of kg is
  signal mfzhel : std_logic;
  signal leee : std_logic;
begin
  bmbwjaw : entity work.dgcwj
    port map (xpn => leee);
  cn : entity work.dgcwj
    port map (xpn => mfzhel);
  
  -- Multi-driven assignments
  mfzhel <= leee;
  leee <= '-';
  mfzhel <= mfzhel;
  mfzhel <= '1';
end yzve;

use std.reflection.all;

entity vjtayqvrnp is
  port (urjbavyv : inout floating_subtype_mirror; gaao : inout subtype_mirror);
end vjtayqvrnp;

architecture vyjjsfdayf of vjtayqvrnp is
  
begin
  
end vyjjsfdayf;

use std.reflection.all;

entity buatlbwm is
  port (rcmzypixs : inout floating_subtype_mirror; emqk : inout subtype_mirror; muqpuf : buffer character; syso : inout integer_value_mirror);
end buatlbwm;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture dgmaug of buatlbwm is
  signal xzgxekuhbh : std_logic;
  signal dl : real;
  shared variable rw : physical_value_mirror;
begin
  zcfnvokcv : entity work.kg
    port map (mwcbyc => rw, aoelf => dl);
  wftod : entity work.dgcwj
    port map (xpn => xzgxekuhbh);
  bzom : entity work.dgcwj
    port map (xpn => xzgxekuhbh);
  dsctkpzk : entity work.vjtayqvrnp
    port map (urjbavyv => rcmzypixs, gaao => emqk);
  
  -- Single-driven assignments
  muqpuf <= 'd';
  
  -- Multi-driven assignments
  xzgxekuhbh <= xzgxekuhbh;
  xzgxekuhbh <= '0';
  xzgxekuhbh <= xzgxekuhbh;
  xzgxekuhbh <= '1';
end dgmaug;



-- Seed after: 11262724865328715354,3566912872917928779
