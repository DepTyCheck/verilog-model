-- Seed: 16073748491285033516,7726014785203345639

use std.reflection.all;

entity sln is
  port (kptuxq : inout access_value_mirror);
end sln;

architecture dkz of sln is
  
begin
  
end dkz;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ls is
  port ( ykzjvcjjuz : inout subtype_mirror
  ; agfkqyd : out std_logic_vector(4 downto 0)
  ; vi : inout protected_subtype_mirror
  ; dfkpltgq : inout array_subtype_mirror
  );
end ls;

use std.reflection.all;

architecture ylalldmq of ls is
  shared variable sbcriyf : access_value_mirror;
begin
  uxyzu : entity work.sln
    port map (kptuxq => sbcriyf);
end ylalldmq;

library ieee;
use ieee.std_logic_1164.all;

entity fztfil is
  port (hgzwfeu : out severity_level; nktfsfxsmx : inout std_logic; uulj : out integer_vector(2 to 4); zx : buffer std_logic);
end fztfil;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

architecture uwaorcjpcr of fztfil is
  shared variable efqrqc : access_value_mirror;
  shared variable wmkkuyrwx : array_subtype_mirror;
  shared variable fgnodlnsy : protected_subtype_mirror;
  signal gthujrwafu : std_logic_vector(4 downto 0);
  shared variable iphdm : subtype_mirror;
  shared variable hnafj : array_subtype_mirror;
  shared variable slwz : protected_subtype_mirror;
  signal mawmn : std_logic_vector(4 downto 0);
  shared variable xfkpmsgvdl : subtype_mirror;
begin
  kbwol : entity work.ls
    port map (ykzjvcjjuz => xfkpmsgvdl, agfkqyd => mawmn, vi => slwz, dfkpltgq => hnafj);
  bqenmg : entity work.ls
    port map (ykzjvcjjuz => iphdm, agfkqyd => gthujrwafu, vi => fgnodlnsy, dfkpltgq => wmkkuyrwx);
  hygxnqqds : entity work.sln
    port map (kptuxq => efqrqc);
  
  -- Single-driven assignments
  uulj <= uulj;
  hgzwfeu <= WARNING;
  
  -- Multi-driven assignments
  gthujrwafu <= "U1HUU";
  zx <= zx;
  gthujrwafu <= "UH--U";
end uwaorcjpcr;



-- Seed after: 14892173245414744110,7726014785203345639
