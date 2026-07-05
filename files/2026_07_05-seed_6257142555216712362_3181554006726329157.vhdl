-- Seed: 6257142555216712362,3181554006726329157

use std.reflection.all;

entity sxtnsljxg is
  port (jlbdpvdd : inout protected_value_mirror; qjzgakri : inout subtype_mirror; ealvnhjacc : inout time; juavdlke : in real);
end sxtnsljxg;

architecture bicczno of sxtnsljxg is
  
begin
  -- Single-driven assignments
  ealvnhjacc <= ealvnhjacc;
end bicczno;

entity pqwnfhcu is
  port (zgukznfu : inout severity_level; lngi : buffer time);
end pqwnfhcu;

use std.reflection.all;

architecture rtlgyd of pqwnfhcu is
  signal xmk : real;
  signal epdkv : time;
  shared variable eblgtm : subtype_mirror;
  shared variable kzgx : protected_value_mirror;
begin
  yxarezvfw : entity work.sxtnsljxg
    port map (jlbdpvdd => kzgx, qjzgakri => eblgtm, ealvnhjacc => epdkv, juavdlke => xmk);
end rtlgyd;

use std.reflection.all;

entity xedrs is
  port (byli : out real; kwhlu : inout value_mirror);
end xedrs;

use std.reflection.all;

architecture rzdzz of xedrs is
  signal plgyxuotp : real;
  signal pi : time;
  shared variable fizy : subtype_mirror;
  shared variable ehas : protected_value_mirror;
  signal woa : time;
  signal hljkyq : severity_level;
  signal d : time;
  shared variable qjk : subtype_mirror;
  shared variable wyzjokezz : protected_value_mirror;
begin
  gmlblx : entity work.sxtnsljxg
    port map (jlbdpvdd => wyzjokezz, qjzgakri => qjk, ealvnhjacc => d, juavdlke => byli);
  gwjryh : entity work.pqwnfhcu
    port map (zgukznfu => hljkyq, lngi => woa);
  benyvgcdz : entity work.sxtnsljxg
    port map (jlbdpvdd => ehas, qjzgakri => fizy, ealvnhjacc => pi, juavdlke => plgyxuotp);
  
  -- Single-driven assignments
  byli <= 12.0_2_1_1;
  plgyxuotp <= byli;
end rzdzz;

use std.reflection.all;

entity kgdah is
  port (wptiknd : out time; clmwheczmz : inout physical_value_mirror);
end kgdah;

use std.reflection.all;

architecture xipm of kgdah is
  shared variable ppujsrb : value_mirror;
  signal mcbjl : real;
begin
  lxhm : entity work.xedrs
    port map (byli => mcbjl, kwhlu => ppujsrb);
  
  -- Single-driven assignments
  wptiknd <= 2#00.0010# ns;
end xipm;



-- Seed after: 16679601440940387523,3181554006726329157
