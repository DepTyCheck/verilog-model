-- Seed: 10402548531062035159,15306146742274979323

entity swl is
  port (ouzas : buffer time; itlikibl : linkage time);
end swl;

architecture q of swl is
  
begin
  -- Single-driven assignments
  ouzas <= 4_4 ps;
end q;

entity sbre is
  port (i : linkage integer; ibnyam : in real; mwfsbzy : buffer boolean; qg : out real);
end sbre;

architecture crvai of sbre is
  signal b : time;
  signal twiov : time;
  signal nluriprq : time;
  signal hcmzt : time;
begin
  id : entity work.swl
    port map (ouzas => hcmzt, itlikibl => nluriprq);
  wpko : entity work.swl
    port map (ouzas => twiov, itlikibl => b);
  
  -- Single-driven assignments
  qg <= qg;
  mwfsbzy <= mwfsbzy;
end crvai;



-- Seed after: 508172993356820294,15306146742274979323
