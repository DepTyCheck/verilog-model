-- Seed: 2223681244012454418,2511821214772927453

entity mbedvhlm is
  port (uslfespo : out real; s : linkage real_vector(0 to 3); fcd : linkage real_vector(4 to 0));
end mbedvhlm;

architecture cvez of mbedvhlm is
  
begin
  -- Single-driven assignments
  uslfespo <= uslfespo;
end cvez;

entity rvx is
  port (orr : buffer time; fvcbsxml : out integer; nwaxl : out integer_vector(4 to 0); elemnseofa : in real);
end rvx;

architecture ybgr of rvx is
  signal hnifbemnfp : real_vector(4 to 0);
  signal wdjjsuta : real_vector(0 to 3);
  signal bhlijch : real;
  signal lqqpy : real_vector(4 to 0);
  signal olt : real_vector(0 to 3);
  signal frpjeqpyqv : real;
begin
  hwxvzxgyiy : entity work.mbedvhlm
    port map (uslfespo => frpjeqpyqv, s => olt, fcd => lqqpy);
  jpxfzisv : entity work.mbedvhlm
    port map (uslfespo => bhlijch, s => wdjjsuta, fcd => hnifbemnfp);
  
  -- Single-driven assignments
  fvcbsxml <= 4;
  orr <= 1 hr;
  nwaxl <= nwaxl;
end ybgr;

entity vsesrhqnq is
  port (ljsnxq : buffer real; fylzx : buffer time);
end vsesrhqnq;

architecture o of vsesrhqnq is
  signal yiljjmz : real_vector(4 to 0);
  signal jpch : real_vector(0 to 3);
  signal pyeugl : real;
  signal oh : real_vector(4 to 0);
  signal xtghicvw : real_vector(0 to 3);
  signal n : real_vector(4 to 0);
  signal ah : real_vector(0 to 3);
  signal xvce : real;
begin
  zvwor : entity work.mbedvhlm
    port map (uslfespo => xvce, s => ah, fcd => n);
  cdk : entity work.mbedvhlm
    port map (uslfespo => ljsnxq, s => xtghicvw, fcd => oh);
  r : entity work.mbedvhlm
    port map (uslfespo => pyeugl, s => jpch, fcd => yiljjmz);
end o;

library ieee;
use ieee.std_logic_1164.all;

entity ljgsesen is
  port (kzlf : inout std_logic; zgzbwuktt : buffer std_logic_vector(3 downto 2); kayhdi : in integer_vector(3 to 4));
end ljgsesen;

architecture zp of ljgsesen is
  signal fbcmgkhbsn : real_vector(4 to 0);
  signal gbvjl : real_vector(0 to 3);
  signal yfe : real;
  signal wf : time;
  signal jnw : real;
  signal va : integer_vector(4 to 0);
  signal fwnrsx : integer;
  signal ew : time;
  signal lmc : real_vector(4 to 0);
  signal x : real_vector(0 to 3);
  signal ethkmh : real;
begin
  xhedcf : entity work.mbedvhlm
    port map (uslfespo => ethkmh, s => x, fcd => lmc);
  txw : entity work.rvx
    port map (orr => ew, fvcbsxml => fwnrsx, nwaxl => va, elemnseofa => jnw);
  owituuz : entity work.vsesrhqnq
    port map (ljsnxq => jnw, fylzx => wf);
  vunbv : entity work.mbedvhlm
    port map (uslfespo => yfe, s => gbvjl, fcd => fbcmgkhbsn);
end zp;



-- Seed after: 15227479580961592618,2511821214772927453
