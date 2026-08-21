-- Seed: 17028003445568301314,16188444798499499427

entity egapvd is
  port (ohipbb : inout time; ia : buffer time);
end egapvd;

architecture usugk of egapvd is
  
begin
  -- Single-driven assignments
  ohipbb <= 2_0_0_3.3042 us;
  ia <= 16#28B# ns;
end usugk;

entity qrkzcn is
  port (aayofwza : inout boolean_vector(3 to 4));
end qrkzcn;

architecture thspuu of qrkzcn is
  signal mhmxqxnrn : time;
  signal bmlztrn : time;
  signal nmqviwx : time;
  signal a : time;
begin
  gxflvin : entity work.egapvd
    port map (ohipbb => a, ia => nmqviwx);
  epdbs : entity work.egapvd
    port map (ohipbb => bmlztrn, ia => mhmxqxnrn);
  
  -- Single-driven assignments
  aayofwza <= aayofwza;
end thspuu;



-- Seed after: 5167555299398506209,16188444798499499427
