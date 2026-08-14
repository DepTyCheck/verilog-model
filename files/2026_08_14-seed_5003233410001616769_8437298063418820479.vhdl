-- Seed: 5003233410001616769,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity aymq is
  port (kxhrbqrsb : inout time; meevnu : out real; mwnh : out std_logic; p : buffer time);
end aymq;

architecture wlzvij of aymq is
  
begin
  -- Single-driven assignments
  kxhrbqrsb <= p;
  p <= p;
  meevnu <= 2#0101.0#;
end wlzvij;

entity zpnmidkqt is
  port (mpskn : out integer; xtlx : out real);
end zpnmidkqt;

library ieee;
use ieee.std_logic_1164.all;

architecture jvxhwjics of zpnmidkqt is
  signal djatvmkwl : time;
  signal iedtaywg : std_logic;
  signal mv : time;
  signal rvzospteu : time;
  signal zrdzwtndwt : real;
  signal dvkdxdayy : time;
  signal uvqf : time;
  signal kjkyrnyqh : real;
  signal kpctvn : time;
  signal yjpfmzppi : time;
  signal ulqjj : std_logic;
  signal rgmelxiwen : real;
  signal qb : time;
begin
  wce : entity work.aymq
    port map (kxhrbqrsb => qb, meevnu => rgmelxiwen, mwnh => ulqjj, p => yjpfmzppi);
  ur : entity work.aymq
    port map (kxhrbqrsb => kpctvn, meevnu => kjkyrnyqh, mwnh => ulqjj, p => uvqf);
  yixyc : entity work.aymq
    port map (kxhrbqrsb => dvkdxdayy, meevnu => zrdzwtndwt, mwnh => ulqjj, p => rvzospteu);
  subd : entity work.aymq
    port map (kxhrbqrsb => mv, meevnu => xtlx, mwnh => iedtaywg, p => djatvmkwl);
  
  -- Single-driven assignments
  mpskn <= mpskn;
end jvxhwjics;



-- Seed after: 9504073691004193646,8437298063418820479
