-- Seed: 16690774962312896068,4245627776430562977

entity awfsle is
  port (b : out time; bqs : linkage integer; djgbu : buffer time; lemhjggit : out string(4 to 1));
end awfsle;

architecture fanpddnd of awfsle is
  
begin
  -- Single-driven assignments
  b <= djgbu;
  lemhjggit <= (others => ' ');
end fanpddnd;

entity xw is
  port (n : in time; txnade : inout real; mxjrewbstp : buffer time);
end xw;

architecture ippzduq of xw is
  signal nfadkszpe : string(4 to 1);
  signal mwy : integer;
  signal irwgzj : time;
  signal eutveq : string(4 to 1);
  signal al : time;
  signal iyibdwpwel : integer;
  signal fx : time;
  signal gzdhplg : string(4 to 1);
  signal chvxfcn : time;
  signal y : integer;
  signal vuziam : time;
  signal dgmuy : string(4 to 1);
  signal ipkvdd : time;
  signal qvfuaj : integer;
  signal gtfhuvrq : time;
begin
  hc : entity work.awfsle
    port map (b => gtfhuvrq, bqs => qvfuaj, djgbu => ipkvdd, lemhjggit => dgmuy);
  wbdepzn : entity work.awfsle
    port map (b => vuziam, bqs => y, djgbu => chvxfcn, lemhjggit => gzdhplg);
  f : entity work.awfsle
    port map (b => fx, bqs => iyibdwpwel, djgbu => al, lemhjggit => eutveq);
  qc : entity work.awfsle
    port map (b => irwgzj, bqs => mwy, djgbu => mxjrewbstp, lemhjggit => nfadkszpe);
  
  -- Single-driven assignments
  txnade <= 2_4_0_1.232;
end ippzduq;

library ieee;
use ieee.std_logic_1164.all;

entity ml is
  port (gvhkd : out std_logic; vpaegjuv : inout time; yybjtxwekw : buffer real_vector(0 to 2));
end ml;

architecture bral of ml is
  signal ih : string(4 to 1);
  signal tu : integer;
  signal bmwtsha : time;
begin
  cfiimipopn : entity work.awfsle
    port map (b => bmwtsha, bqs => tu, djgbu => vpaegjuv, lemhjggit => ih);
  
  -- Single-driven assignments
  yybjtxwekw <= yybjtxwekw;
  
  -- Multi-driven assignments
  gvhkd <= gvhkd;
end bral;



-- Seed after: 12452313977630203478,4245627776430562977
