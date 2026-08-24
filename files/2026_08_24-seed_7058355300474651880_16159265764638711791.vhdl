-- Seed: 7058355300474651880,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity wrdnwfjj is
  port (qomg : buffer time; favjgtm : linkage std_logic);
end wrdnwfjj;

architecture nhmp of wrdnwfjj is
  
begin
  -- Single-driven assignments
  qomg <= 8#46# ms;
end nhmp;

library ieee;
use ieee.std_logic_1164.all;

entity djwquumx is
  port (wqrb : linkage time; sfzfvmzrdq : out std_logic_vector(4 downto 3); xtzkgx : inout std_logic);
end djwquumx;

architecture vggc of djwquumx is
  signal wsojkahe : time;
begin
  yjfezcjh : entity work.wrdnwfjj
    port map (qomg => wsojkahe, favjgtm => xtzkgx);
  
  -- Multi-driven assignments
  xtzkgx <= 'Z';
  xtzkgx <= 'U';
end vggc;

library ieee;
use ieee.std_logic_1164.all;

entity xjnunhthlj is
  port (lodnktgota : buffer std_logic; txdrfwe : inout std_logic_vector(0 to 1));
end xjnunhthlj;

architecture xlgejonvif of xjnunhthlj is
  signal eaxrr : time;
  signal nqunfkliv : time;
  signal bphm : time;
  signal vxsndhjc : time;
begin
  m : entity work.wrdnwfjj
    port map (qomg => vxsndhjc, favjgtm => lodnktgota);
  mbibwf : entity work.wrdnwfjj
    port map (qomg => bphm, favjgtm => lodnktgota);
  xinz : entity work.djwquumx
    port map (wqrb => nqunfkliv, sfzfvmzrdq => txdrfwe, xtzkgx => lodnktgota);
  oewsd : entity work.wrdnwfjj
    port map (qomg => eaxrr, favjgtm => lodnktgota);
end xlgejonvif;



-- Seed after: 14417598246199662432,16159265764638711791
