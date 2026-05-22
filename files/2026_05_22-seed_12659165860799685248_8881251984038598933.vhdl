-- Seed: 12659165860799685248,8881251984038598933



entity ostorbolzc is
  port (jezc : linkage time; vpkvnvoha : linkage integer; nf : in real);
end ostorbolzc;



architecture dhhvubzuoe of ostorbolzc is
  
begin
  
end dhhvubzuoe;



entity svgxeh is
  port (gfi : inout integer; zusnihlqiq : linkage real; bjh : inout real);
end svgxeh;



architecture rtbdpygoe of svgxeh is
  signal aeyntefgzm : integer;
  signal povuvjcso : time;
begin
  fznukavgo : entity work.ostorbolzc
    port map (jezc => povuvjcso, vpkvnvoha => aeyntefgzm, nf => bjh);
end rtbdpygoe;



entity xvzkzl is
  port (ecwmrnxfcr : out integer);
end xvzkzl;



architecture irno of xvzkzl is
  signal elkwl : time;
  signal kwbrcrstue : real;
  signal eadwoaj : time;
  signal zcpxnp : real;
  signal tkl : real;
  signal czrjkxz : time;
begin
  y : entity work.ostorbolzc
    port map (jezc => czrjkxz, vpkvnvoha => ecwmrnxfcr, nf => tkl);
  um : entity work.ostorbolzc
    port map (jezc => czrjkxz, vpkvnvoha => ecwmrnxfcr, nf => zcpxnp);
  r : entity work.ostorbolzc
    port map (jezc => eadwoaj, vpkvnvoha => ecwmrnxfcr, nf => kwbrcrstue);
  zidwjobdh : entity work.ostorbolzc
    port map (jezc => elkwl, vpkvnvoha => ecwmrnxfcr, nf => kwbrcrstue);
end irno;

library ieee;
use ieee.std_logic_1164.all;

entity zlhaw is
  port (oblllgo : linkage std_logic; hugr : buffer character; lnnaofauv : inout time);
end zlhaw;



architecture aagfyizpf of zlhaw is
  signal athnbor : real;
  signal ttewdobx : integer;
  signal vvg : time;
  signal xnylflg : real;
  signal hvwgpxzj : integer;
begin
  bzbz : entity work.ostorbolzc
    port map (jezc => lnnaofauv, vpkvnvoha => hvwgpxzj, nf => xnylflg);
  gchi : entity work.ostorbolzc
    port map (jezc => vvg, vpkvnvoha => ttewdobx, nf => athnbor);
  ckyvlow : entity work.ostorbolzc
    port map (jezc => lnnaofauv, vpkvnvoha => ttewdobx, nf => xnylflg);
end aagfyizpf;



-- Seed after: 12307892531457503774,8881251984038598933
