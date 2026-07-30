-- Seed: 5736153679401655634,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity jl is
  port (rfyhxwy : linkage std_logic; qgozpynrn : out std_logic_vector(1 to 4));
end jl;

architecture brp of jl is
  
begin
  -- Multi-driven assignments
  qgozpynrn <= qgozpynrn;
end brp;

entity ksgn is
  port (uh : buffer boolean);
end ksgn;

library ieee;
use ieee.std_logic_1164.all;

architecture bdvaag of ksgn is
  signal gvjsfmypc : std_logic_vector(1 to 4);
  signal jrozrplmz : std_logic_vector(1 to 4);
  signal jzg : std_logic;
  signal bjrsagzl : std_logic_vector(1 to 4);
  signal tlmy : std_logic;
begin
  jtdkusvfyu : entity work.jl
    port map (rfyhxwy => tlmy, qgozpynrn => bjrsagzl);
  d : entity work.jl
    port map (rfyhxwy => jzg, qgozpynrn => jrozrplmz);
  rb : entity work.jl
    port map (rfyhxwy => jzg, qgozpynrn => gvjsfmypc);
  
  -- Single-driven assignments
  uh <= uh;
  
  -- Multi-driven assignments
  tlmy <= 'W';
end bdvaag;

entity tuq is
  port (gspns : inout time_vector(2 downto 3); dw : in real; ssqjrkla : linkage real);
end tuq;

architecture rdrhdg of tuq is
  
begin
  -- Single-driven assignments
  gspns <= gspns;
end rdrhdg;

entity ujucjc is
  port (vaqmwfmp : inout severity_level; vpls : inout integer);
end ujucjc;

library ieee;
use ieee.std_logic_1164.all;

architecture kz of ujucjc is
  signal oibrpj : std_logic_vector(1 to 4);
  signal hndjjs : std_logic;
  signal nolugt : real;
  signal jzjjnk : time_vector(2 downto 3);
begin
  rf : entity work.tuq
    port map (gspns => jzjjnk, dw => nolugt, ssqjrkla => nolugt);
  pnwnsj : entity work.jl
    port map (rfyhxwy => hndjjs, qgozpynrn => oibrpj);
  
  -- Single-driven assignments
  vpls <= vpls;
  vaqmwfmp <= vaqmwfmp;
  
  -- Multi-driven assignments
  hndjjs <= hndjjs;
end kz;



-- Seed after: 4266687974867427026,4122021602305298647
