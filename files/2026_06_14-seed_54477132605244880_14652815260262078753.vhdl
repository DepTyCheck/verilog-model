-- Seed: 54477132605244880,14652815260262078753

entity p is
  port (ejz : inout integer);
end p;

architecture lyieeyueh of p is
  
begin
  -- Single-driven assignments
  ejz <= 30044;
end lyieeyueh;

entity yvsckuh is
  port (dhmqhz : inout integer; bgsij : inout time; jpmhc : inout severity_level);
end yvsckuh;

architecture diopgj of yvsckuh is
  signal qqnirham : integer;
  signal ekyhghud : integer;
begin
  frtltr : entity work.p
    port map (ejz => ekyhghud);
  hrlzegqzxl : entity work.p
    port map (ejz => qqnirham);
  okqzjaii : entity work.p
    port map (ejz => dhmqhz);
  
  -- Single-driven assignments
  jpmhc <= WARNING;
  bgsij <= 22.1 ms;
end diopgj;

library ieee;
use ieee.std_logic_1164.all;

entity yyxrt is
  port (jthdzv : inout time; srd : linkage integer; czqzpifn : linkage std_logic_vector(3 downto 1));
end yyxrt;

architecture rhgkzlx of yyxrt is
  signal oymprd : integer;
begin
  dhabg : entity work.p
    port map (ejz => oymprd);
  
  -- Single-driven assignments
  jthdzv <= 16#4046A.7661# ms;
end rhgkzlx;

library ieee;
use ieee.std_logic_1164.all;

entity thyfofcwpc is
  port (qmgtikqxi : inout std_logic);
end thyfofcwpc;

library ieee;
use ieee.std_logic_1164.all;

architecture xyp of thyfofcwpc is
  signal vmcbfkfqc : std_logic_vector(3 downto 1);
  signal my : integer;
  signal vrnhrepel : time;
  signal ixubadds : integer;
  signal tfoiqlodzr : time;
  signal yr : std_logic_vector(3 downto 1);
  signal wikbfpdbi : integer;
  signal vgkquncfhd : time;
  signal a : integer;
begin
  aflxzrhv : entity work.p
    port map (ejz => a);
  ly : entity work.yyxrt
    port map (jthdzv => vgkquncfhd, srd => wikbfpdbi, czqzpifn => yr);
  tndchioli : entity work.yyxrt
    port map (jthdzv => tfoiqlodzr, srd => ixubadds, czqzpifn => yr);
  fjdvoxfax : entity work.yyxrt
    port map (jthdzv => vrnhrepel, srd => my, czqzpifn => vmcbfkfqc);
  
  -- Multi-driven assignments
  yr <= "--Z";
  qmgtikqxi <= 'L';
  qmgtikqxi <= 'Z';
  qmgtikqxi <= 'L';
end xyp;



-- Seed after: 18291835569447574907,14652815260262078753
