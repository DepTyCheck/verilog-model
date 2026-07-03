-- Seed: 15359304200566095483,2158184632809654795

use std.reflection.all;

entity nbyqgr is
  port (nlcgrin : inout physical_value_mirror; odqjoxtn : out severity_level);
end nbyqgr;

architecture oly of nbyqgr is
  
begin
  -- Single-driven assignments
  odqjoxtn <= FAILURE;
end oly;

use std.reflection.all;

entity kisabmptk is
  port (m : inout enumeration_value_mirror; w : inout time);
end kisabmptk;

use std.reflection.all;

architecture iukvs of kisabmptk is
  signal cthcqcjvr : severity_level;
  shared variable rgfkuw : physical_value_mirror;
  signal pjuopkvtpf : severity_level;
  shared variable bqjvhfa : physical_value_mirror;
begin
  xwzipoe : entity work.nbyqgr
    port map (nlcgrin => bqjvhfa, odqjoxtn => pjuopkvtpf);
  onsfippfca : entity work.nbyqgr
    port map (nlcgrin => rgfkuw, odqjoxtn => cthcqcjvr);
end iukvs;

library ieee;
use ieee.std_logic_1164.all;
use std.reflection.all;

entity ib is
  port (ved : inout record_value_mirror; szdjfxpstm : buffer std_logic_vector(0 to 2); cvbdm : linkage real);
end ib;

use std.reflection.all;

architecture thm of ib is
  signal pzdibni : severity_level;
  shared variable teejxjxqo : physical_value_mirror;
  signal aamihsso : severity_level;
  shared variable bzys : physical_value_mirror;
  signal h : severity_level;
  shared variable gxlgjlf : physical_value_mirror;
begin
  c : entity work.nbyqgr
    port map (nlcgrin => gxlgjlf, odqjoxtn => h);
  ehlbrkzwq : entity work.nbyqgr
    port map (nlcgrin => bzys, odqjoxtn => aamihsso);
  xsnzwio : entity work.nbyqgr
    port map (nlcgrin => teejxjxqo, odqjoxtn => pzdibni);
end thm;

library ieee;
use ieee.std_logic_1164.all;

entity jzl is
  port (aczednldd : out std_logic_vector(0 to 0));
end jzl;

use std.reflection.all;

architecture jjdxoza of jzl is
  signal koorrbyd : severity_level;
  shared variable s : physical_value_mirror;
  signal sxr : time;
  shared variable phqjte : enumeration_value_mirror;
  signal canmhpl : severity_level;
  shared variable kafn : physical_value_mirror;
begin
  mwif : entity work.nbyqgr
    port map (nlcgrin => kafn, odqjoxtn => canmhpl);
  nn : entity work.kisabmptk
    port map (m => phqjte, w => sxr);
  dkpkyfnqum : entity work.nbyqgr
    port map (nlcgrin => s, odqjoxtn => koorrbyd);
  
  -- Multi-driven assignments
  aczednldd <= (others => 'L');
end jjdxoza;



-- Seed after: 7142783512262802891,2158184632809654795
