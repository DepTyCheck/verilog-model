-- Seed: 4717930963204829298,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity daizcb is
  port (aihbkxf : in std_logic_vector(2 downto 2); hwjlg : linkage std_logic; fkimsx : out boolean; n : inout integer);
end daizcb;

architecture esmr of daizcb is
  
begin
  -- Single-driven assignments
  n <= 10033;
  fkimsx <= TRUE;
end esmr;

library ieee;
use ieee.std_logic_1164.all;

entity ulawm is
  port (drr : in boolean_vector(1 downto 0); gl : in std_logic_vector(2 downto 2));
end ulawm;

library ieee;
use ieee.std_logic_1164.all;

architecture bd of ulawm is
  signal zdc : integer;
  signal puyfqb : boolean;
  signal ldbrpjhc : std_logic;
  signal ejyncflut : integer;
  signal oywcacr : boolean;
  signal vsa : integer;
  signal e : boolean;
  signal tfvowciw : std_logic;
begin
  qezhl : entity work.daizcb
    port map (aihbkxf => gl, hwjlg => tfvowciw, fkimsx => e, n => vsa);
  qlw : entity work.daizcb
    port map (aihbkxf => gl, hwjlg => tfvowciw, fkimsx => oywcacr, n => ejyncflut);
  tmalsfah : entity work.daizcb
    port map (aihbkxf => gl, hwjlg => ldbrpjhc, fkimsx => puyfqb, n => zdc);
  
  -- Multi-driven assignments
  tfvowciw <= 'U';
end bd;

library ieee;
use ieee.std_logic_1164.all;

entity sujclepuq is
  port (ze : buffer std_logic_vector(3 to 0); j : buffer time; wquvsrq : out std_logic; ncs : in real);
end sujclepuq;

library ieee;
use ieee.std_logic_1164.all;

architecture g of sujclepuq is
  signal gmsrcfmyuk : integer;
  signal oee : boolean;
  signal kejbzeluqu : std_logic_vector(2 downto 2);
  signal f : integer;
  signal kxavrpxlg : boolean;
  signal evjppgaqu : std_logic_vector(2 downto 2);
  signal shbc : std_logic_vector(2 downto 2);
  signal zhkvpb : boolean_vector(1 downto 0);
begin
  wzce : entity work.ulawm
    port map (drr => zhkvpb, gl => shbc);
  cbw : entity work.daizcb
    port map (aihbkxf => evjppgaqu, hwjlg => wquvsrq, fkimsx => kxavrpxlg, n => f);
  ysc : entity work.daizcb
    port map (aihbkxf => kejbzeluqu, hwjlg => wquvsrq, fkimsx => oee, n => gmsrcfmyuk);
  
  -- Single-driven assignments
  j <= 16#D.F47CA# ps;
  
  -- Multi-driven assignments
  wquvsrq <= '1';
  kejbzeluqu <= (others => '1');
  kejbzeluqu <= (others => 'L');
end g;

library ieee;
use ieee.std_logic_1164.all;

entity bqxnlqnhw is
  port (k : linkage std_logic_vector(0 downto 1); wngc : buffer integer; wxz : inout std_logic_vector(3 downto 4));
end bqxnlqnhw;

library ieee;
use ieee.std_logic_1164.all;

architecture hvu of bqxnlqnhw is
  signal vwfrcie : real;
  signal vc : time;
  signal kkdzp : std_logic_vector(3 to 0);
  signal kwlkxcasdd : real;
  signal phmi : time;
  signal yjmpt : boolean;
  signal avp : std_logic;
  signal b : std_logic_vector(2 downto 2);
begin
  m : entity work.daizcb
    port map (aihbkxf => b, hwjlg => avp, fkimsx => yjmpt, n => wngc);
  rkwu : entity work.sujclepuq
    port map (ze => wxz, j => phmi, wquvsrq => avp, ncs => kwlkxcasdd);
  nyguk : entity work.sujclepuq
    port map (ze => kkdzp, j => vc, wquvsrq => avp, ncs => vwfrcie);
  
  -- Multi-driven assignments
  wxz <= (others => '0');
  wxz <= "";
  avp <= 'U';
end hvu;



-- Seed after: 73631176900605104,8421704836678237495
