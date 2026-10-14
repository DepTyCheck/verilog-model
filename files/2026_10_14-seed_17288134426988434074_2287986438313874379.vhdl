-- Seed: 17288134426988434074,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity cbecjdtr is
  port (lwkfhxc : buffer std_logic_vector(0 to 3); wyitb : inout integer; fmaxpywoeg : buffer integer);
end cbecjdtr;

architecture c of cbecjdtr is
  
begin
  -- Single-driven assignments
  fmaxpywoeg <= 1;
  wyitb <= fmaxpywoeg;
  
  -- Multi-driven assignments
  lwkfhxc <= lwkfhxc;
  lwkfhxc <= lwkfhxc;
  lwkfhxc <= lwkfhxc;
  lwkfhxc <= lwkfhxc;
end c;

entity ijzuo is
  port (i : linkage boolean_vector(4 downto 3));
end ijzuo;

library ieee;
use ieee.std_logic_1164.all;

architecture tgquplosmo of ijzuo is
  signal bzl : integer;
  signal meatagq : integer;
  signal inmu : std_logic_vector(0 to 3);
begin
  qu : entity work.cbecjdtr
    port map (lwkfhxc => inmu, wyitb => meatagq, fmaxpywoeg => bzl);
end tgquplosmo;

entity jdc is
  port (ggtjhtfzr : in integer; kxlbk : linkage real; jrohcp : out bit; rstnm : in time_vector(2 downto 1));
end jdc;

library ieee;
use ieee.std_logic_1164.all;

architecture w of jdc is
  signal fsebahj : integer;
  signal ztrxkj : integer;
  signal digo : std_logic_vector(0 to 3);
  signal wjcehyw : boolean_vector(4 downto 3);
  signal f : integer;
  signal igzjk : integer;
  signal lrhfj : integer;
  signal cjq : integer;
  signal rtjtjhhsf : std_logic_vector(0 to 3);
begin
  ert : entity work.cbecjdtr
    port map (lwkfhxc => rtjtjhhsf, wyitb => cjq, fmaxpywoeg => lrhfj);
  yx : entity work.cbecjdtr
    port map (lwkfhxc => rtjtjhhsf, wyitb => igzjk, fmaxpywoeg => f);
  uhfhutfo : entity work.ijzuo
    port map (i => wjcehyw);
  uiqsfe : entity work.cbecjdtr
    port map (lwkfhxc => digo, wyitb => ztrxkj, fmaxpywoeg => fsebahj);
  
  -- Single-driven assignments
  jrohcp <= jrohcp;
  
  -- Multi-driven assignments
  rtjtjhhsf <= "01-X";
  rtjtjhhsf <= ('L', 'X', 'H', '1');
  digo <= rtjtjhhsf;
end w;



-- Seed after: 6965438362657410017,2287986438313874379
