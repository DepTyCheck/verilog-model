-- Seed: 18184833614175897190,3687118713772291287

entity q is
  port (vjybxnree : inout time; ednxemizt : inout bit; immpanl : buffer boolean_vector(2 to 1));
end q;

architecture ctv of q is
  
begin
  
end ctv;

entity iyassznhi is
  port (dakeqhgg : buffer bit; dxabfqna : inout real_vector(3 downto 3); biixnlrsnr : out real; kdkyfpjjus : in real);
end iyassznhi;

architecture aho of iyassznhi is
  signal k : boolean_vector(2 to 1);
  signal oseowh : bit;
  signal mqy : time;
  signal aovhpgxm : boolean_vector(2 to 1);
  signal xkhp : bit;
  signal bszospef : time;
  signal manin : boolean_vector(2 to 1);
  signal tycizhouqv : time;
begin
  lmmybef : entity work.q
    port map (vjybxnree => tycizhouqv, ednxemizt => dakeqhgg, immpanl => manin);
  gso : entity work.q
    port map (vjybxnree => bszospef, ednxemizt => xkhp, immpanl => aovhpgxm);
  zccvgxeg : entity work.q
    port map (vjybxnree => mqy, ednxemizt => oseowh, immpanl => k);
end aho;



-- Seed after: 11780745440422049672,3687118713772291287
