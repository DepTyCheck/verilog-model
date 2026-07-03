-- Seed: 6555531987822854076,2158184632809654795

use std.reflection.all;

entity bicljbwbmf is
  port (ikhokq : inout floating_value_mirror; ortx : out integer);
end bicljbwbmf;

architecture nzytizwrd of bicljbwbmf is
  
begin
  -- Single-driven assignments
  ortx <= 1_2;
end nzytizwrd;

entity ffse is
  port (hqkapeths : in boolean_vector(4 to 4));
end ffse;

use std.reflection.all;

architecture raujpe of ffse is
  signal ruo : integer;
  shared variable cn : floating_value_mirror;
begin
  uyshjru : entity work.bicljbwbmf
    port map (ikhokq => cn, ortx => ruo);
end raujpe;

use std.reflection.all;

entity qhlybyqw is
  port (wnwkan : buffer time; dgyzbbgkb : inout enumeration_subtype_mirror; amzahg : inout access_subtype_mirror; xfpw : in character);
end qhlybyqw;

use std.reflection.all;

architecture gkngozl of qhlybyqw is
  signal h : boolean_vector(4 to 4);
  signal puxqzrt : integer;
  shared variable ire : floating_value_mirror;
  signal rapmnuyf : integer;
  shared variable gmhazcd : floating_value_mirror;
begin
  zj : entity work.bicljbwbmf
    port map (ikhokq => gmhazcd, ortx => rapmnuyf);
  zngek : entity work.bicljbwbmf
    port map (ikhokq => ire, ortx => puxqzrt);
  dyhq : entity work.ffse
    port map (hqkapeths => h);
  
  -- Single-driven assignments
  wnwkan <= wnwkan;
  h <= (others => FALSE);
end gkngozl;



-- Seed after: 9358173823791164142,2158184632809654795
