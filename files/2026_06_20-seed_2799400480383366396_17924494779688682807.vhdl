-- Seed: 2799400480383366396,17924494779688682807

entity xxujqzkbj is
  port (a : in severity_level; ernebf : inout real);
end xxujqzkbj;

architecture paqzh of xxujqzkbj is
  
begin
  -- Single-driven assignments
  ernebf <= 2#1.0_1#;
end paqzh;

entity u is
  port (nvi : out bit);
end u;

architecture kdnqtvo of u is
  signal n : real;
  signal yaxtur : severity_level;
  signal seymv : real;
  signal s : severity_level;
  signal amsg : real;
  signal wosirtsd : severity_level;
  signal ouajwyrark : real;
  signal ytuezsmdtf : severity_level;
begin
  rdifbj : entity work.xxujqzkbj
    port map (a => ytuezsmdtf, ernebf => ouajwyrark);
  ewemjp : entity work.xxujqzkbj
    port map (a => wosirtsd, ernebf => amsg);
  gy : entity work.xxujqzkbj
    port map (a => s, ernebf => seymv);
  vocvttry : entity work.xxujqzkbj
    port map (a => yaxtur, ernebf => n);
  
  -- Single-driven assignments
  ytuezsmdtf <= FAILURE;
  yaxtur <= NOTE;
end kdnqtvo;

entity ykdmcvt is
  port (usjrydmtgr : inout time_vector(2 to 3));
end ykdmcvt;

architecture swvwv of ykdmcvt is
  signal qjovoupuy : real;
  signal rmkkftg : bit;
  signal dvtpdwj : real;
  signal mgs : severity_level;
  signal cnom : bit;
begin
  zncbogc : entity work.u
    port map (nvi => cnom);
  jzzlfmbqn : entity work.xxujqzkbj
    port map (a => mgs, ernebf => dvtpdwj);
  pqikoob : entity work.u
    port map (nvi => rmkkftg);
  spmehoemwr : entity work.xxujqzkbj
    port map (a => mgs, ernebf => qjovoupuy);
end swvwv;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (dsbobt : in std_logic_vector(4 downto 2); fmki : linkage time_vector(1 to 3));
end k;

architecture fg of k is
  signal mda : time_vector(2 to 3);
begin
  c : entity work.ykdmcvt
    port map (usjrydmtgr => mda);
end fg;



-- Seed after: 14852893178974614155,17924494779688682807
