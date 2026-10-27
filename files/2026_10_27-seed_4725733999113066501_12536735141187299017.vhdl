-- Seed: 4725733999113066501,12536735141187299017

entity s is
  port (tpxs : out time; lzikqplxcu : inout string(4 downto 5); jf : inout real; pf : linkage real);
end s;

architecture vbppsj of s is
  
begin
  -- Single-driven assignments
  jf <= jf;
  tpxs <= 2#0_0_0_1_0# us;
  lzikqplxcu <= "";
end vbppsj;

entity nhjxn is
  port (zjbiytuos : buffer real; rytpplc : in bit; sbtemddkr : inout real; vosvcmmg : inout integer);
end nhjxn;

architecture pw of nhjxn is
  signal ajukli : real;
  signal kzajpla : real;
  signal imhokzuiv : string(4 downto 5);
  signal ffix : time;
  signal gigo : string(4 downto 5);
  signal cvwlqznsu : time;
begin
  bzidb : entity work.s
    port map (tpxs => cvwlqznsu, lzikqplxcu => gigo, jf => zjbiytuos, pf => sbtemddkr);
  gybgmlktgm : entity work.s
    port map (tpxs => ffix, lzikqplxcu => imhokzuiv, jf => kzajpla, pf => ajukli);
end pw;

entity zhzyyekrh is
  port (qdq : linkage string(3 downto 5); nbyoty : in real);
end zhzyyekrh;

architecture niyivxp of zhzyyekrh is
  signal rayeqffg : real;
  signal gefnkevtq : real;
  signal fosbecsmbv : string(4 downto 5);
  signal jnrrsmz : time;
  signal wanfp : real;
  signal ab : real;
  signal zjgpxtkq : string(4 downto 5);
  signal ocakb : time;
  signal viupqd : real;
  signal xeymywcnml : real;
  signal wlxrlzyy : string(4 downto 5);
  signal nibp : time;
begin
  uvuaghkd : entity work.s
    port map (tpxs => nibp, lzikqplxcu => wlxrlzyy, jf => xeymywcnml, pf => viupqd);
  musmauqu : entity work.s
    port map (tpxs => ocakb, lzikqplxcu => zjgpxtkq, jf => ab, pf => wanfp);
  ixezotj : entity work.s
    port map (tpxs => jnrrsmz, lzikqplxcu => fosbecsmbv, jf => gefnkevtq, pf => rayeqffg);
end niyivxp;

library ieee;
use ieee.std_logic_1164.all;

entity wzfwfmjq is
  port (rf : inout severity_level; q : buffer integer; hlytnesl : buffer std_logic; juypinnua : out std_logic);
end wzfwfmjq;

architecture iwg of wzfwfmjq is
  signal hzkfmsx : real;
  signal hscisuf : bit;
  signal zb : real;
  signal ekvprvrf : real;
  signal pkyizaizf : real;
  signal ovfrzu : string(4 downto 5);
  signal gv : time;
  signal xtnohu : real;
  signal qtmz : real;
  signal fptpihn : string(4 downto 5);
  signal szlu : time;
  signal shwgcpfwhq : real;
  signal anwltdqg : real;
  signal esp : string(4 downto 5);
  signal jxdacemp : time;
begin
  hlj : entity work.s
    port map (tpxs => jxdacemp, lzikqplxcu => esp, jf => anwltdqg, pf => shwgcpfwhq);
  ftrx : entity work.s
    port map (tpxs => szlu, lzikqplxcu => fptpihn, jf => qtmz, pf => xtnohu);
  bjhq : entity work.s
    port map (tpxs => gv, lzikqplxcu => ovfrzu, jf => pkyizaizf, pf => ekvprvrf);
  qkr : entity work.nhjxn
    port map (zjbiytuos => zb, rytpplc => hscisuf, sbtemddkr => hzkfmsx, vosvcmmg => q);
  
  -- Single-driven assignments
  rf <= ERROR;
  hscisuf <= '1';
  
  -- Multi-driven assignments
  juypinnua <= juypinnua;
  juypinnua <= 'X';
end iwg;



-- Seed after: 17905219647038172944,12536735141187299017
