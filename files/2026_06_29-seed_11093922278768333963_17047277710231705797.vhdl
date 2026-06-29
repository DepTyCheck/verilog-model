-- Seed: 11093922278768333963,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity ciy is
  port (stqlsvnz : out real; kp : buffer time; vkpljoyme : inout std_logic_vector(1 to 2); abclsa : in boolean_vector(0 to 3));
end ciy;

architecture zty of ciy is
  
begin
  -- Single-driven assignments
  stqlsvnz <= 034.4_2_3;
  kp <= 312.04200 ps;
  
  -- Multi-driven assignments
  vkpljoyme <= ('-', '1');
  vkpljoyme <= "XX";
  vkpljoyme <= "-0";
  vkpljoyme <= "0-";
end zty;

library ieee;
use ieee.std_logic_1164.all;

entity gegeklpdvl is
  port (dtjpzxb : buffer std_logic_vector(0 to 4); pgbojgvpzn : inout integer; vyjapwrbuk : out real; waqtdzx : buffer real);
end gegeklpdvl;

library ieee;
use ieee.std_logic_1164.all;

architecture bmdypafr of gegeklpdvl is
  signal aslb : boolean_vector(0 to 3);
  signal fosn : time;
  signal ldybjrjrt : real;
  signal oix : std_logic_vector(1 to 2);
  signal uuahpmmng : time;
  signal pgpfalbb : real;
  signal rhhcomdr : boolean_vector(0 to 3);
  signal pexzy : std_logic_vector(1 to 2);
  signal eiybrx : time;
  signal qrri : real;
begin
  jaeapvbod : entity work.ciy
    port map (stqlsvnz => qrri, kp => eiybrx, vkpljoyme => pexzy, abclsa => rhhcomdr);
  gat : entity work.ciy
    port map (stqlsvnz => pgpfalbb, kp => uuahpmmng, vkpljoyme => oix, abclsa => rhhcomdr);
  fhnyvs : entity work.ciy
    port map (stqlsvnz => ldybjrjrt, kp => fosn, vkpljoyme => pexzy, abclsa => aslb);
  
  -- Single-driven assignments
  aslb <= (FALSE, FALSE, TRUE, TRUE);
  waqtdzx <= 4030.1_4_0_4_3;
  vyjapwrbuk <= 16#E.B3AB#;
  pgbojgvpzn <= 8#3#;
  
  -- Multi-driven assignments
  oix <= ('L', 'U');
end bmdypafr;

library ieee;
use ieee.std_logic_1164.all;

entity lz is
  port (ibu : out std_logic; m : out time; djtjkgdur : linkage std_logic);
end lz;

library ieee;
use ieee.std_logic_1164.all;

architecture ocz of lz is
  signal jqhpkreyq : time;
  signal ltijgdkj : real;
  signal xwstfmacky : real;
  signal yxetxoih : real;
  signal pezouxemdx : integer;
  signal dccbkdikid : std_logic_vector(0 to 4);
  signal y : boolean_vector(0 to 3);
  signal yhaot : std_logic_vector(1 to 2);
  signal ica : time;
  signal fayst : real;
begin
  xfny : entity work.ciy
    port map (stqlsvnz => fayst, kp => ica, vkpljoyme => yhaot, abclsa => y);
  r : entity work.gegeklpdvl
    port map (dtjpzxb => dccbkdikid, pgbojgvpzn => pezouxemdx, vyjapwrbuk => yxetxoih, waqtdzx => xwstfmacky);
  w : entity work.ciy
    port map (stqlsvnz => ltijgdkj, kp => jqhpkreyq, vkpljoyme => yhaot, abclsa => y);
  
  -- Single-driven assignments
  y <= (FALSE, TRUE, FALSE, TRUE);
  m <= 1 min;
  
  -- Multi-driven assignments
  yhaot <= ('Z', 'H');
  ibu <= 'W';
  ibu <= '-';
  dccbkdikid <= ('H', '0', '-', '-', 'Z');
end ocz;

entity wwmy is
  port (dpeaymerz : out bit; fkdjddorzu : out time; jrljhozu : buffer boolean);
end wwmy;

architecture olx of wwmy is
  
begin
  -- Single-driven assignments
  jrljhozu <= TRUE;
  dpeaymerz <= '1';
  fkdjddorzu <= 16#CCD2.6_6# us;
end olx;



-- Seed after: 11975275009098171972,17047277710231705797
