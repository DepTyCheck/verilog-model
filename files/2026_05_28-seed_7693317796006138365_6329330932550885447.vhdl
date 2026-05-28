-- Seed: 7693317796006138365,6329330932550885447



entity tbi is
  port (vkxpn : inout boolean_vector(4 to 0); ly : buffer integer; lvclqgvhs : inout bit);
end tbi;



architecture qwnyst of tbi is
  
begin
  
end qwnyst;



entity g is
  port (mrantdnt : out time);
end g;



architecture dftmuuqpvr of g is
  signal gzvn : bit;
  signal klncadtn : integer;
  signal aqvs : boolean_vector(4 to 0);
  signal rfesl : bit;
  signal byenilleo : integer;
  signal ddnjqi : boolean_vector(4 to 0);
  signal hy : bit;
  signal nbcyszli : integer;
  signal l : boolean_vector(4 to 0);
begin
  p : entity work.tbi
    port map (vkxpn => l, ly => nbcyszli, lvclqgvhs => hy);
  uyxrbk : entity work.tbi
    port map (vkxpn => ddnjqi, ly => byenilleo, lvclqgvhs => rfesl);
  stftzxcfq : entity work.tbi
    port map (vkxpn => aqvs, ly => klncadtn, lvclqgvhs => gzvn);
end dftmuuqpvr;



entity hxel is
  port (nwckq : out bit_vector(0 downto 1));
end hxel;



architecture qycthr of hxel is
  signal r : bit;
  signal m : integer;
  signal yhepl : boolean_vector(4 to 0);
  signal lsotvxkk : bit;
  signal jjjoybn : integer;
  signal ppsfmskscb : boolean_vector(4 to 0);
  signal lziiut : bit;
  signal banti : integer;
  signal vl : boolean_vector(4 to 0);
  signal reoc : time;
begin
  sfo : entity work.g
    port map (mrantdnt => reoc);
  b : entity work.tbi
    port map (vkxpn => vl, ly => banti, lvclqgvhs => lziiut);
  pqup : entity work.tbi
    port map (vkxpn => ppsfmskscb, ly => jjjoybn, lvclqgvhs => lsotvxkk);
  ceucreglqc : entity work.tbi
    port map (vkxpn => yhepl, ly => m, lvclqgvhs => r);
end qycthr;



-- Seed after: 7904573840659816327,6329330932550885447
