-- Seed: 17752096254894825796,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity fus is
  port (itgsue : buffer bit_vector(3 downto 2); somaw : in std_logic; wxbapbqc : linkage bit_vector(2 to 3));
end fus;

architecture aklrnnhe of fus is
  
begin
  -- Single-driven assignments
  itgsue <= itgsue;
end aklrnnhe;

entity fvz is
  port (lts : inout bit; cpnzjjjdrw : linkage integer; ejiovjzj : inout time; enefrrd : out real_vector(2 downto 4));
end fvz;

library ieee;
use ieee.std_logic_1164.all;

architecture qfzqjolmb of fvz is
  signal awd : bit_vector(2 to 3);
  signal nakimaiaen : std_logic;
  signal ks : bit_vector(3 downto 2);
  signal avkyprkdkv : bit_vector(2 to 3);
  signal lwgmzqvyj : bit_vector(3 downto 2);
  signal okjubrkd : bit_vector(2 to 3);
  signal xbtuth : std_logic;
  signal ye : bit_vector(3 downto 2);
  signal png : bit_vector(2 to 3);
  signal guob : std_logic;
  signal f : bit_vector(3 downto 2);
begin
  mylu : entity work.fus
    port map (itgsue => f, somaw => guob, wxbapbqc => png);
  glhgxc : entity work.fus
    port map (itgsue => ye, somaw => xbtuth, wxbapbqc => okjubrkd);
  nhasxgvo : entity work.fus
    port map (itgsue => lwgmzqvyj, somaw => guob, wxbapbqc => avkyprkdkv);
  fprwxc : entity work.fus
    port map (itgsue => ks, somaw => nakimaiaen, wxbapbqc => awd);
end qfzqjolmb;

entity oqoddbroc is
  port (ahszcsrc : out real; lnjstwpz : linkage real);
end oqoddbroc;

library ieee;
use ieee.std_logic_1164.all;

architecture rndflmnsae of oqoddbroc is
  signal gzms : bit_vector(2 to 3);
  signal ptyxegubr : bit_vector(3 downto 2);
  signal ezl : bit_vector(2 to 3);
  signal xvbzaemgm : bit_vector(3 downto 2);
  signal zdwugm : bit_vector(2 to 3);
  signal pfx : std_logic;
  signal cy : bit_vector(3 downto 2);
begin
  ev : entity work.fus
    port map (itgsue => cy, somaw => pfx, wxbapbqc => zdwugm);
  omthe : entity work.fus
    port map (itgsue => xvbzaemgm, somaw => pfx, wxbapbqc => ezl);
  ki : entity work.fus
    port map (itgsue => ptyxegubr, somaw => pfx, wxbapbqc => gzms);
  
  -- Multi-driven assignments
  pfx <= '-';
  pfx <= pfx;
  pfx <= pfx;
end rndflmnsae;

library ieee;
use ieee.std_logic_1164.all;

entity ginrrykint is
  port (xmb : inout integer_vector(4 downto 2); qrv : in std_logic);
end ginrrykint;

library ieee;
use ieee.std_logic_1164.all;

architecture dqxutagmwi of ginrrykint is
  signal xhmuws : real_vector(2 downto 4);
  signal oqdbeodrm : time;
  signal sar : integer;
  signal njyaumhzif : bit;
  signal vbctgw : bit_vector(2 to 3);
  signal xtoclqwz : std_logic;
  signal etthxgi : bit_vector(3 downto 2);
begin
  uulh : entity work.fus
    port map (itgsue => etthxgi, somaw => xtoclqwz, wxbapbqc => vbctgw);
  jfvpzuyh : entity work.fvz
    port map (lts => njyaumhzif, cpnzjjjdrw => sar, ejiovjzj => oqdbeodrm, enefrrd => xhmuws);
  
  -- Single-driven assignments
  xmb <= xmb;
  
  -- Multi-driven assignments
  xtoclqwz <= xtoclqwz;
  xtoclqwz <= qrv;
end dqxutagmwi;



-- Seed after: 15002887509842814243,16159265764638711791
