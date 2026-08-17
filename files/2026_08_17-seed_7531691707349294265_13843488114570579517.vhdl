-- Seed: 7531691707349294265,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity fcqeh is
  port (rezlf : in severity_level; txxkpqfw : buffer std_logic_vector(3 to 0); jyqcgnn : out std_logic_vector(0 downto 2); hrjuq : in real);
end fcqeh;

architecture dboqwyf of fcqeh is
  
begin
  -- Multi-driven assignments
  jyqcgnn <= "";
end dboqwyf;

entity slqy is
  port (xygffokmof : inout time; a : buffer boolean);
end slqy;

library ieee;
use ieee.std_logic_1164.all;

architecture uwjgkcrv of slqy is
  signal rti : std_logic_vector(0 downto 2);
  signal wnslkspj : std_logic_vector(3 to 0);
  signal ov : severity_level;
  signal lqp : real;
  signal y : std_logic_vector(0 downto 2);
  signal msbsqio : std_logic_vector(3 to 0);
  signal pkqkydytco : severity_level;
begin
  ljfhcpcwxh : entity work.fcqeh
    port map (rezlf => pkqkydytco, txxkpqfw => msbsqio, jyqcgnn => y, hrjuq => lqp);
  jsebffhdhi : entity work.fcqeh
    port map (rezlf => ov, txxkpqfw => wnslkspj, jyqcgnn => rti, hrjuq => lqp);
end uwjgkcrv;

library ieee;
use ieee.std_logic_1164.all;

entity nffw is
  port (nolzpw : out std_logic; zudyr : buffer real_vector(0 to 1); fmj : buffer std_logic_vector(2 to 2); fu : out std_logic);
end nffw;

library ieee;
use ieee.std_logic_1164.all;

architecture qmxi of nffw is
  signal s : std_logic_vector(0 downto 2);
  signal sd : std_logic_vector(0 downto 2);
  signal mckjr : std_logic_vector(3 to 0);
  signal qnrbjvx : real;
  signal yf : real;
  signal ekfjvdotlz : std_logic_vector(3 to 0);
  signal iuyoc : std_logic_vector(0 downto 2);
  signal hkj : severity_level;
begin
  awytvmg : entity work.fcqeh
    port map (rezlf => hkj, txxkpqfw => iuyoc, jyqcgnn => ekfjvdotlz, hrjuq => yf);
  odvxpa : entity work.fcqeh
    port map (rezlf => hkj, txxkpqfw => iuyoc, jyqcgnn => iuyoc, hrjuq => qnrbjvx);
  vfpyznpx : entity work.fcqeh
    port map (rezlf => hkj, txxkpqfw => mckjr, jyqcgnn => sd, hrjuq => yf);
  kpetxhtvdj : entity work.fcqeh
    port map (rezlf => hkj, txxkpqfw => ekfjvdotlz, jyqcgnn => s, hrjuq => yf);
end qmxi;

entity wtc is
  port (hhknoxpym : out boolean);
end wtc;

library ieee;
use ieee.std_logic_1164.all;

architecture yzmrx of wtc is
  signal omydykd : real;
  signal d : std_logic_vector(0 downto 2);
  signal oyxfktr : std_logic_vector(3 to 0);
  signal xqchlqmp : severity_level;
  signal wcvppop : real;
  signal jvgrmipkuq : std_logic_vector(0 downto 2);
  signal rnmekcufbx : severity_level;
begin
  th : entity work.fcqeh
    port map (rezlf => rnmekcufbx, txxkpqfw => jvgrmipkuq, jyqcgnn => jvgrmipkuq, hrjuq => wcvppop);
  bmvg : entity work.fcqeh
    port map (rezlf => xqchlqmp, txxkpqfw => oyxfktr, jyqcgnn => d, hrjuq => omydykd);
  
  -- Single-driven assignments
  omydykd <= wcvppop;
  hhknoxpym <= hhknoxpym;
  rnmekcufbx <= rnmekcufbx;
  xqchlqmp <= xqchlqmp;
  
  -- Multi-driven assignments
  jvgrmipkuq <= jvgrmipkuq;
  oyxfktr <= "";
end yzmrx;



-- Seed after: 12512527877484714307,13843488114570579517
