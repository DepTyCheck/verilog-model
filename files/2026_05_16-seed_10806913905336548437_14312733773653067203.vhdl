-- Seed: 10806913905336548437,14312733773653067203



entity o is
  port (cumcpjf : in integer; gzvdslpwx : in time; ihh : in time);
end o;



architecture vihqtrdip of o is
  
begin
  
end vihqtrdip;

library ieee;
use ieee.std_logic_1164.all;

entity sdc is
  port (eij : out std_logic);
end sdc;



architecture fbkglhf of sdc is
  signal dpqeivpmlk : time;
  signal akzyfi : integer;
  signal ysufow : time;
  signal oaj : integer;
  signal ebqs : time;
  signal iqxcmn : time;
  signal mgqntn : time;
  signal sfhfmppndo : integer;
begin
  rjdkrgwgs : entity work.o
    port map (cumcpjf => sfhfmppndo, gzvdslpwx => mgqntn, ihh => iqxcmn);
  dxvzwccb : entity work.o
    port map (cumcpjf => sfhfmppndo, gzvdslpwx => mgqntn, ihh => ebqs);
  f : entity work.o
    port map (cumcpjf => oaj, gzvdslpwx => mgqntn, ihh => ysufow);
  q : entity work.o
    port map (cumcpjf => akzyfi, gzvdslpwx => mgqntn, ihh => dpqeivpmlk);
end fbkglhf;

library ieee;
use ieee.std_logic_1164.all;

entity bor is
  port (jax : out std_logic; et : in std_logic; jo : linkage time; hhwrpwv : buffer integer);
end bor;



architecture hytlsxqjy of bor is
  signal dx : time;
  signal cjqpmr : time;
  signal dpcwg : time;
begin
  oei : entity work.o
    port map (cumcpjf => hhwrpwv, gzvdslpwx => dpcwg, ihh => cjqpmr);
  jwhylva : entity work.o
    port map (cumcpjf => hhwrpwv, gzvdslpwx => cjqpmr, ihh => dx);
  wprbgkji : entity work.sdc
    port map (eij => jax);
end hytlsxqjy;



-- Seed after: 36811181439061688,14312733773653067203
