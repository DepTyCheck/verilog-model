-- Seed: 17425999485919652562,3687118713772291287

entity isfs is
  port (rdiu : inout boolean; botatqtuy : out real_vector(2 downto 2));
end isfs;

architecture yjxbqpavz of isfs is
  
begin
  -- Single-driven assignments
  botatqtuy <= (others => 8#325.7_3_0#);
  rdiu <= TRUE;
end yjxbqpavz;

entity wsznmts is
  port (pibaywuki : in real; xscwoeilrr : out boolean_vector(3 downto 3); cipnbqmbxm : in integer; dn : linkage real_vector(4 to 4));
end wsznmts;

architecture wzjdputsv of wsznmts is
  signal xiuabc : real_vector(2 downto 2);
  signal lvq : boolean;
  signal spj : real_vector(2 downto 2);
  signal rcuho : boolean;
  signal gilm : real_vector(2 downto 2);
  signal gtbyhgl : boolean;
  signal wmjv : real_vector(2 downto 2);
  signal oqasf : boolean;
begin
  ot : entity work.isfs
    port map (rdiu => oqasf, botatqtuy => wmjv);
  psyr : entity work.isfs
    port map (rdiu => gtbyhgl, botatqtuy => gilm);
  rai : entity work.isfs
    port map (rdiu => rcuho, botatqtuy => spj);
  v : entity work.isfs
    port map (rdiu => lvq, botatqtuy => xiuabc);
  
  -- Single-driven assignments
  xscwoeilrr <= (others => FALSE);
end wzjdputsv;

library ieee;
use ieee.std_logic_1164.all;

entity jbauuqsaf is
  port (fqzq : inout std_logic_vector(4 to 4); exh : in integer; fgkhiw : in integer; mqmuyupoci : out real);
end jbauuqsaf;

architecture eavwamrnlq of jbauuqsaf is
  signal whysu : real_vector(4 to 4);
  signal mxkwodcwna : integer;
  signal wmv : boolean_vector(3 downto 3);
  signal lv : real_vector(2 downto 2);
  signal djn : boolean;
  signal izffurv : real_vector(2 downto 2);
  signal uwi : boolean;
begin
  erpgprnw : entity work.isfs
    port map (rdiu => uwi, botatqtuy => izffurv);
  shyg : entity work.isfs
    port map (rdiu => djn, botatqtuy => lv);
  jeizl : entity work.wsznmts
    port map (pibaywuki => mqmuyupoci, xscwoeilrr => wmv, cipnbqmbxm => mxkwodcwna, dn => whysu);
  
  -- Single-driven assignments
  mqmuyupoci <= 8#3.660#;
  mxkwodcwna <= 34;
  
  -- Multi-driven assignments
  fqzq <= (others => 'H');
  fqzq <= (others => 'U');
  fqzq <= (others => '-');
end eavwamrnlq;



-- Seed after: 3584524578486319317,3687118713772291287
