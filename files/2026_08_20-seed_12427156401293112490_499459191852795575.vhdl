-- Seed: 12427156401293112490,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity jl is
  port (cwahe : buffer time_vector(0 to 0); yxfrtoyymp : out std_logic);
end jl;

architecture lbj of jl is
  
begin
  -- Single-driven assignments
  cwahe <= cwahe;
  
  -- Multi-driven assignments
  yxfrtoyymp <= 'X';
  yxfrtoyymp <= '1';
end lbj;

library ieee;
use ieee.std_logic_1164.all;

entity zupg is
  port (hjxyenq : out std_logic; baexkbolxp : out std_logic);
end zupg;

library ieee;
use ieee.std_logic_1164.all;

architecture jdfhsznqhv of zupg is
  signal yxa : std_logic;
  signal fd : time_vector(0 to 0);
  signal ltrkve : time_vector(0 to 0);
  signal yzbdlec : time_vector(0 to 0);
begin
  onknd : entity work.jl
    port map (cwahe => yzbdlec, yxfrtoyymp => baexkbolxp);
  hp : entity work.jl
    port map (cwahe => ltrkve, yxfrtoyymp => hjxyenq);
  dbwirmdd : entity work.jl
    port map (cwahe => fd, yxfrtoyymp => yxa);
  
  -- Multi-driven assignments
  baexkbolxp <= '1';
end jdfhsznqhv;

entity ywobakq is
  port (rr : buffer integer_vector(4 to 0); oyhngx : buffer time; ku : buffer real);
end ywobakq;

architecture dfza of ywobakq is
  
begin
  -- Single-driven assignments
  ku <= ku;
  rr <= rr;
end dfza;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (gfvozbo : buffer std_logic_vector(2 downto 4); plhkhkg : inout severity_level; koa : inout std_logic);
end v;

architecture nccdy of v is
  signal ehhbki : real;
  signal qxyvlwc : time;
  signal zb : integer_vector(4 to 0);
  signal uljhdys : time_vector(0 to 0);
begin
  ge : entity work.jl
    port map (cwahe => uljhdys, yxfrtoyymp => koa);
  ummqqblrv : entity work.ywobakq
    port map (rr => zb, oyhngx => qxyvlwc, ku => ehhbki);
  
  -- Single-driven assignments
  plhkhkg <= plhkhkg;
  
  -- Multi-driven assignments
  koa <= '0';
end nccdy;



-- Seed after: 5415507235490832190,499459191852795575
