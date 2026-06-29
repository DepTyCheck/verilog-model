-- Seed: 5750297950365051088,17047277710231705797

entity cgdtxhv is
  port (uqx : linkage integer; eoxi : linkage real; nua : buffer integer);
end cgdtxhv;

architecture f of cgdtxhv is
  
begin
  
end f;

entity pkxwrdz is
  port (rgjbzwnx : inout real_vector(1 to 3); titpd : buffer integer; q : in boolean);
end pkxwrdz;

architecture ivemi of pkxwrdz is
  signal yxjorzy : real;
  signal otmxs : integer;
begin
  maxuiwtex : entity work.cgdtxhv
    port map (uqx => otmxs, eoxi => yxjorzy, nua => titpd);
  
  -- Single-driven assignments
  rgjbzwnx <= (16#2_7.C_9_5_C_4#, 2#1.10#, 243.4);
end ivemi;

entity ajkooiwmu is
  port (b : buffer integer);
end ajkooiwmu;

architecture xozorbgwga of ajkooiwmu is
  signal enx : boolean;
  signal cbq : integer;
  signal xugceyqq : real_vector(1 to 3);
begin
  jgoelldwro : entity work.pkxwrdz
    port map (rgjbzwnx => xugceyqq, titpd => cbq, q => enx);
  
  -- Single-driven assignments
  b <= 2#1_0_0_0#;
  enx <= TRUE;
end xozorbgwga;



-- Seed after: 12126627780754530828,17047277710231705797
