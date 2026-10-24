-- Seed: 6599107718510459718,10975587770517081373

entity ifezg is
  port (dcdxlxlrih : inout boolean);
end ifezg;

architecture qbzjuz of ifezg is
  
begin
  -- Single-driven assignments
  dcdxlxlrih <= FALSE;
end qbzjuz;

entity pzh is
  port (qcecbmfx : inout severity_level);
end pzh;

architecture fwpi of pzh is
  signal imwh : boolean;
  signal ald : boolean;
  signal om : boolean;
begin
  tugg : entity work.ifezg
    port map (dcdxlxlrih => om);
  gigjkrv : entity work.ifezg
    port map (dcdxlxlrih => ald);
  p : entity work.ifezg
    port map (dcdxlxlrih => imwh);
  
  -- Single-driven assignments
  qcecbmfx <= qcecbmfx;
end fwpi;

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (njg : buffer std_logic; fpowwe : buffer std_logic_vector(3 downto 3); upb : inout std_logic_vector(2 to 2));
end p;

architecture sozatdbl of p is
  signal j : boolean;
  signal edqubztmmn : severity_level;
begin
  ibe : entity work.pzh
    port map (qcecbmfx => edqubztmmn);
  qcnqynwugn : entity work.ifezg
    port map (dcdxlxlrih => j);
  
  -- Multi-driven assignments
  fpowwe <= "U";
end sozatdbl;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (hpmqil : inout integer; wcthd : linkage std_logic; st : in time; hzpcqtmyyv : inout time);
end q;

architecture vv of q is
  signal uiqyzgdwv : boolean;
  signal uqetjr : boolean;
  signal ziznmsvkw : boolean;
  signal xfgo : severity_level;
begin
  caohddwvh : entity work.pzh
    port map (qcecbmfx => xfgo);
  zisotp : entity work.ifezg
    port map (dcdxlxlrih => ziznmsvkw);
  kqd : entity work.ifezg
    port map (dcdxlxlrih => uqetjr);
  mylmdkoo : entity work.ifezg
    port map (dcdxlxlrih => uiqyzgdwv);
  
  -- Single-driven assignments
  hzpcqtmyyv <= 8#53# fs;
end vv;



-- Seed after: 5407566938039342621,10975587770517081373
