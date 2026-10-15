-- Seed: 9991959137074484463,6377581586043967497

entity uszpc is
  port (dsseqalv : in real; uwokxdbvy : in time; eewejh : out boolean);
end uszpc;

architecture aelw of uszpc is
  
begin
  -- Single-driven assignments
  eewejh <= TRUE;
end aelw;

entity owsnysiblt is
  port (o : out time; nsk : in real; guijvtylni : inout integer; u : inout integer);
end owsnysiblt;

architecture e of owsnysiblt is
  signal xc : boolean;
  signal rizt : boolean;
  signal dr : boolean;
  signal yvydcbexs : real;
begin
  zg : entity work.uszpc
    port map (dsseqalv => yvydcbexs, uwokxdbvy => o, eewejh => dr);
  dgoo : entity work.uszpc
    port map (dsseqalv => yvydcbexs, uwokxdbvy => o, eewejh => rizt);
  yjixpddh : entity work.uszpc
    port map (dsseqalv => nsk, uwokxdbvy => o, eewejh => xc);
  
  -- Single-driven assignments
  u <= guijvtylni;
  yvydcbexs <= 2#1.01110#;
end e;

entity cf is
  port (mkanlek : in time; pjeuqxo : linkage bit);
end cf;

architecture fraqr of cf is
  
begin
  
end fraqr;



-- Seed after: 16672478761686788836,6377581586043967497
