-- Seed: 8988341974943504679,10557070023141912087

entity mwtp is
  port (avzj : in integer; mfyjba : inout boolean; nmp : linkage time; xwcotkvbee : inout integer);
end mwtp;

architecture pvbyuh of mwtp is
  
begin
  -- Single-driven assignments
  xwcotkvbee <= 1002;
end pvbyuh;

entity x is
  port (yws : inout integer; rz : buffer integer);
end x;

architecture dno of x is
  signal jiv : time;
  signal bihijet : boolean;
  signal jj : integer;
  signal wlpzb : time;
  signal qudnp : boolean;
begin
  wlt : entity work.mwtp
    port map (avzj => yws, mfyjba => qudnp, nmp => wlpzb, xwcotkvbee => jj);
  xtuoqt : entity work.mwtp
    port map (avzj => rz, mfyjba => bihijet, nmp => jiv, xwcotkvbee => rz);
end dno;



-- Seed after: 1664124329665578988,10557070023141912087
