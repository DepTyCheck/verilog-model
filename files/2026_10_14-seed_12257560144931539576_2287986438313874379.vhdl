-- Seed: 12257560144931539576,2287986438313874379

entity uohpu is
  port (beket : out time);
end uohpu;

architecture xfmsx of uohpu is
  
begin
  -- Single-driven assignments
  beket <= 1 hr;
end xfmsx;

entity gv is
  port (pkm : in time_vector(1 downto 1));
end gv;

architecture ogv of gv is
  signal lxime : time;
  signal tmuzet : time;
begin
  lqywsk : entity work.uohpu
    port map (beket => tmuzet);
  dgbgjwl : entity work.uohpu
    port map (beket => lxime);
end ogv;

library ieee;
use ieee.std_logic_1164.all;

entity oxapouspgv is
  port (szqv : in std_logic_vector(0 to 0); lvibdvqucv : inout real_vector(4 downto 2); j : inout real);
end oxapouspgv;

architecture fsndiwtcuf of oxapouspgv is
  signal ws : time;
begin
  ruilmt : entity work.uohpu
    port map (beket => ws);
end fsndiwtcuf;

library ieee;
use ieee.std_logic_1164.all;

entity ekcumgde is
  port (jhvxc : inout std_logic);
end ekcumgde;

library ieee;
use ieee.std_logic_1164.all;

architecture qbj of ekcumgde is
  signal sgajtyncb : real;
  signal isbmf : real_vector(4 downto 2);
  signal hegnnj : std_logic_vector(0 to 0);
begin
  exwpkmjp : entity work.oxapouspgv
    port map (szqv => hegnnj, lvibdvqucv => isbmf, j => sgajtyncb);
  
  -- Multi-driven assignments
  jhvxc <= jhvxc;
  jhvxc <= '1';
end qbj;



-- Seed after: 17358250611770688521,2287986438313874379
