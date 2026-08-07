-- Seed: 1485809955714449555,8068158652091157513

library ieee;
use ieee.std_logic_1164.all;

entity rx is
  port (ckty : buffer severity_level; ssgiq : buffer std_logic);
end rx;

architecture cwccc of rx is
  
begin
  -- Single-driven assignments
  ckty <= WARNING;
  
  -- Multi-driven assignments
  ssgiq <= ssgiq;
end cwccc;

entity hdrpj is
  port (fboqwkxe : linkage boolean; ali : inout time);
end hdrpj;

library ieee;
use ieee.std_logic_1164.all;

architecture gpm of hdrpj is
  signal pcbj : std_logic;
  signal poxxgx : severity_level;
  signal ef : std_logic;
  signal bcrjdjjre : severity_level;
  signal wn : std_logic;
  signal mh : severity_level;
begin
  bnwsxycd : entity work.rx
    port map (ckty => mh, ssgiq => wn);
  hca : entity work.rx
    port map (ckty => bcrjdjjre, ssgiq => ef);
  qtfp : entity work.rx
    port map (ckty => poxxgx, ssgiq => pcbj);
  
  -- Single-driven assignments
  ali <= 2#100# us;
  
  -- Multi-driven assignments
  wn <= '-';
  ef <= wn;
end gpm;



-- Seed after: 15129090578083937104,8068158652091157513
